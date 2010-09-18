Amelie - Haskell paste web site
===============================

About
-----

This is a paste web service. It supports:

* Syntax highlighting for various languages.
* Choosing the associated channel.
* Annotations.
* Preview.
* Browsing pastes.
* Completely themable, the presentation is separated from the logic.
* Pretty URLs.

Setup Instructions
------------------

* Install PostgreSQL server and development modules.
* Install libfcgi and development modules.
* Install Takusen with the -fpostgres flag, this is important.
* Install nginx or some other FastCGI-capable server. This
* package includes a handy nginx configuration file.

Go to the extracted amelie/ package directory.

$ cabal install takusen -fpostgres
$ cabal install

You can alternatively configure and build instead of installing,
but this installs the dependencies for you.

Create a postgresql role and database for `amelie'. Then import
the initial database:

$ pg_restore -c -d amelie -U amelie amelie.dump

Copy the amelie.conf.example to amelie.conf and fill in the
database details.

Start the server:

$ cabal test nginx start
$ cabal test fastcgi start

Go to your web browser and visit: http://127.0.0.1:8000/

Tada!
