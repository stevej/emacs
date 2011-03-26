(setq path "/bin:/usr/local/bin:/usr/local/mysql/bin:/usr/bin:/usr/sbin:/Users/stevej/bin:/opt/local/bin:/Users/stevej/local/ensime:/Users/stevej/local/scala-2.8.1.final/bin")
(setenv "PATH" path)


(setq exec-path (append exec-path (split-string path ":")))
