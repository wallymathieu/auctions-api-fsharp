# auctions-site [![Build Status](https://travis-ci.org/wallymathieu/auctions-site.svg?branch=master)](https://travis-ci.org/wallymathieu/auctions-site) [![Build status](https://ci.appveyor.com/api/projects/status/wwefc0io4oh2wnrf/branch/master?svg=true)](https://ci.appveyor.com/project/wallymathieu/auctions-site/branch/master)

Auctions site implemented in f# with f#+, Redis, Fleece and Suave

## Technical overview

### Assumptions

- agent is a `Job.foreverServer`
- each auction get 1 agent
- all commands (once they are authorized) are logged in redis, json et.c. (even commands that are rejected)
- one dedicated agent for command persisters (json, redis)

A more complete implementation could have supervisors, circuit breakers, retries et.c..

### High level overview of command and query flow

```md
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
dotnet run --project App --redis CONN --json FILE --web-hook URI
```

To try out the auction API you can then curl the service:

```bash
AUCTIONS_TOKEN_SELLER=`echo '{"sub":"a1", "name":"Seller", "u_typ":"0"}' | base64`
# eyJzdWIiOiJhMSIsICJuYW1lIjoiU2VsbGVyIiwgInVfdHlwIjoiMCJ9Cg==
AUCTIONS_TOKEN_BUYER=`echo '{"sub":"a2", "name":"Buyer", "u_typ":"0"}' | base64`
# eyJzdWIiOiJhMiIsICJuYW1lIjoiQnV5ZXIiLCAidV90eXAiOiIwIn0K
curl  -X POST -d '{ "id":1,"startsAt":"2018-01-01T10:00:00.000Z","endsAt":"2019-01-01T10:00:00.000Z","title":"First auction", "currency":"VAC" }' -H "x-jwt-payload: $AUCTIONS_TOKEN_SELLER"  -H "Content-Type: application/json"  127.0.0.1:8083/auction
curl  -X POST -d '{ "amount":"VAC10" }' -H "x-jwt-payload: $AUCTIONS_TOKEN_BUYER"  -H "Content-Type: application/json"  127.0.0.1:8083/auction/1/bid
curl  -X GET -H "x-jwt-payload: $AUCTIONS_TOKEN_SELLER"  -H "Content-Type: application/json"  127.0.0.1:8083/auctions
```
