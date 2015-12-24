
require('doRedis')

redisWorker(queue="jobs",
            host = "192.168.xxx.xxx",
            port = 6379,
            iter = Inf,
            timeout = 30,
            log = stdout(),
            connected=FALSE,
            password=NULL)


