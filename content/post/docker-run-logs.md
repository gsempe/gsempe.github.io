---
description: "How to dominate Docker when you use it sparsely"
keywords:
  - docker
  - dockerfile
categories:
  - Quick Tips
tags:
  - devops
  - docker

title: "Docker's first aid for software developers"
slug: "Docker Tips Software Developers"
date: 2017-10-23T05:45:26+01:00
lastmod: 2017-10-28T16:16:16+01:00
draft: false
---

<sup>***Note:*** *If you have as well some tricks to cover you when you use Docker, I'll be pleased to add them*</sup> 

As a software developer, I work with Docker quite rarely. I create at the beginning of a project:

- the scripts to build the needed custom docker images
- the scripts to launch quickly the docker containers
- and that's  almost all.

If you're like me, you probably forget quickly the Docker way or the tips and after 30 mins of déjà vu digging, you get frustrated.

## Debug docker run when the container terminates immediately

Launch your container with the `docker run` command

```batch
docker run -d --name cdadb --rm -p 5432:5432 pgsql
a0bfa9fa74b66ddd1c2242f169fc508d47c83263ac92667cd8adaa7bbc1c9ae4
```

Then check if it's running with the `docker ps` command
```batch
docker ps
CONTAINER ID        IMAGE               COMMAND             CREATED             STATUS              PORTS               NAMES
```

If your container is not listed above, we have the same problem. Our containers are not running, probably terminated during the initialization steps because of an error in the customization.

To find what happened, give the `docker run` command result to the `docker log` command. The `docker run` command print the ID of the container it creates and so one gives it as a parameter to the `docker log` command

```batch
docker logs -f $(docker run -d --name $BASENAME --rm -p 5432:5432 $BASENAME-backend)
```

```sh
The files belonging to this database system will be owned by user "postgres".
This user must also own the server process.

[...]

/usr/local/bin/docker-entrypoint.sh: running /docker-entrypoint-initdb.d/01_setup_db_dump.sql

[...]

/usr/local/bin/docker-entrypoint.sh: running /docker-entrypoint-initdb.d/02_backup_dump.sh
pg_restore: [archiver] input file appears to be a text format dump. Please use psql.
```

Et voilà! The last line printed by the container logs tells me that I use the dump of the postgres database in a wrong way. I should use `psql` instead of `pg_restore`.

This trick is applicable to all images. I describe it with the postgres image because it's my personal case.

## Free your SSD space, delete docker history

After few months of usage and if you never clean, projects after projects, Docker is literally eating disk space.

Remove all the containers from your machine

```batch
docker rm -f $(docker ps -a -q)
```

Remove all the images from your machine

```batch
docker rmi -f $(docker images -q)
```
