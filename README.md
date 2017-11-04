# auctions-site [![Build Status](https://travis-ci.org/wallymathieu/auctions-site.svg?branch=master)](https://travis-ci.org/wallymathieu/auctions-site) [![Build status](https://ci.appveyor.com/api/projects/status/wwefc0io4oh2wnrf/branch/master?svg=true)](https://ci.appveyor.com/project/wallymathieu/auctions-site/branch/master)

Auctions site implemented in f# with redis

### Assumptions:

- each auction get 1 agent 
- all commands (once they are authorized) are logged in redis, json et.c. (even commands that are rejected)
- one dedicated thread for command persisters (json, redis)


```
signal -> delegator -> agents.[x] 

command    -> agents.[x] --> auction listeners
           |
           \-> persisters.[...] -> persist command


query      -> query agent.[y] --> Result<QueryResult,QueryError>
```
