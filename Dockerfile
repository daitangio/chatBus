FROM erlang:24
# Remve old rebar
RUN rm /usr/local/bin/rebar
RUN mkdir -p /var/log/erlang/chat_server
