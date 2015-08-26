PROJECT = erwa_test

DEPS = erwa awre
dep_erwa = git https://github.com/bwegh/erwa.git master
dep_awre = git https://github.com/bwegh/awre.git master

include erlang.mk
