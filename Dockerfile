#FROM erlang:24
FROM erlang:23
# Remve old rebar
RUN rm /usr/local/bin/rebar
