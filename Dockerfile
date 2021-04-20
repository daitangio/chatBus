FROM erlang:24
# Remve old rebar
RUN rm /usr/local/bin/rebar
RUN mkdir -p /var/log/erlang/chat_server
RUN apt update && apt-get install -y sqlite3=3.27.2-3+deb10u1
