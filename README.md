# auctions-site [![Build Status](https://travis-ci.org/wallymathieu/auctions-site.svg?branch=master)](https://travis-ci.org/wallymathieu/auctions-site) [![Build status](https://ci.appveyor.com/api/projects/status/wwefc0io4oh2wnrf/branch/master?svg=true)](https://ci.appveyor.com/project/wallymathieu/auctions-site/branch/master)

Auctions site implemented in f# with f#+, Redis, Fleece and Suave

## Technical overview

### Assumptions

- each auction get 1 mailbox
- all commands (once they are authorized) are logged in redis, json et.c. (even commands that are rejected)
- one dedicated mailbox for command persisters (json, redis)
- exceptions in mailbox causes the entire program to exit with non zero exit code

A more complete implementation could have supervisors, circuit breakers, retries et.c..

### High level overview of command and query flow

```
signal -> delegator -> mailbox.[x]

command ------[handle]---> mailbox.[x] --[observe result]-\
           |                                               v
           |----------------[observe command]-----> observers.[...]
           |
           \---[persist command]--> persisters.[...]


query ----[query]---> mailbox.[y] --[return]--> Result<QueryResult,QueryError>
```

## Running it

If you run the project without any parameters, auctions will be in memory only. In order to persist commands you will need to provide either a
connection string to redis or file to persist commands in.

```bash
dotnet restore
dotnet run --project Auctions --redis CONN --json FILE --web-hook URI
```

To try out the auction API you can then curl the service:

```bash
AUCTION_TOKEN=`echo '{"sub":"a1", "name":"Test", "u_typ":"0"}' | base64`
# i.e.   eyJzdWIiOiJhMSIsICJuYW1lIjoiVGVzdCIsICJ1X3R5cCI6IjAifQo=
curl  -X POST -d '{ "id":1,"startsAt":"2018-01-01T10:00:00.000Z","endsAt":"2019-01-01T10:00:00.000Z","title":"First auction", "currency":"VAC" }' -H "x-jwt-payload: $AUCTION_TOKEN"  -H "Content-Type: application/json"  127.0.0.1:8083/auction
curl  -X POST -d '{ "amount":"VAC10" }' -H "x-jwt-payload: $AUCTION_TOKEN"  -H "Content-Type: application/json"  127.0.0.1:8083/auction/1/bid
curl  -X GET -H "x-jwt-payload: $AUCTION_TOKEN"  -H "Content-Type: application/json"  127.0.0.1:8083/auctions
```

