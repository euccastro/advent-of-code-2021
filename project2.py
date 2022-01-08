import hypercorn
import quart
import quart_trio
import os
import trio

app = quart_trio.QuartTrio("chat_app")

history = []

@app.get("/")
async def get():
    return quart.jsonify(history)

conns = {}
names = {}

async def send_line(args):
    conn_id, name, line = args
    conn = conns.get(conn_id)
    if conn:
        await conn.send((name or "?") + ": " + line)

@app.websocket("/ws")
async def ws():
    s = quart.websocket._get_current_object()
    sid = id(s)
    conns[sid] = s
    print("conns is", conns)
    try:
        while True:
            line = (await s.receive()).strip()
            words = line.split()
            if words[0] == '/login':
                if len(words) == 2:
                    names[sid] = words[1]
                else:
                    await s.send('Usage: /login <username>')
            elif words[0] == '/quit':
                await quart.websocket.send("BYE!")
                await quart.websocket.close(200)
                break
            else:
                name = names.get(sid)
                history.append(line)
                async with trio.open_nursery() as nursery:
                    for conn_id in conns:
                        nursery.start_soon(send_line, (conn_id, name, line))
    finally:
        names.pop(sid)
        del conns[sid]

@trio.run
async def main(*, task_status=trio.TASK_STATUS_IGNORED):
    print("~~~ Starting up! ~~~")
    # On Heroku, have to bind to whatever $PORT says:
    # https://devcenter.heroku.com/articles/dynos#local-environment-variables
    port = os.environ.get("PORT", 8000)
    async with trio.open_nursery() as nursery:
        config = hypercorn.Config.from_mapping(
            bind=[f"0.0.0.0:{port}"],
            # Log to stdout
            accesslog="-",
            errorlog="-",
            # Setting this just silences a warning:
            worker_class="trio",
        )
        urls = await nursery.start(hypercorn.trio.serve, app, config)
        print("Accepting HTTP requests at:", urls)
        task_status.started(urls)
