# yt-indexer

tired of youtube recommendations?

want more flexibility to query videos of a certain topic?

run this shit 💩.

# Features

some features at a glance...

* indexing recent youtube videos to sql lite
* easy configuration with json
* multiple keywords to search on 

# How To Use

## Intro

this utility can be run to index youtube videos that **you** care about and save them into a SQLite database for later querying.
to view the indexed results you'll need an application that can query such as [DB Browser](https://sqlitebrowser.org/) _(not affiliated, but I like the program)_.

## Running

if you're not using the pre-built binary, a few requirements to run the program you'll need to get:

* [SQLite library for your platform](https://sqlite.org/download.html)
* and open ssl installed 
  * binaries (dll for windows) can also sit next to the binary

also, before running you'll need to configure the settings to your liking:

the `appsettings.json` file controls things such as query quota, database name, and topics you're interested in indexing.
below is an example json file that you can use (this needs to be named `appsettings.json` and sit in the same directory as yt_indexer)

```json
{
  "yt_indexer": {
    "db_file_name": "yt_indexer.db",
    "quota_cost": 100,
    "daily_quota": 10000,
    "remaining_quota": 10000,
    "api_key": "YOUR_API_KEY",
    "keywords_csv": "KEYWORD_1, KEYWORD_2, KEYWORDS CAN BE PHRASES TOO JUST SEPARATE BY A COMMA"
  }
}
```

_note: running yt_indexer will also generate the appsettings.json file the first time with defaults, but creating first is fine too_

## Building From Source

1. download and install lazarus if you don't already have it (http://www.lazarus-ide.org)
    * you'll need the latest fpc with decorator support
1. git clone this repo
1. open yt_indexer.lpr and attempt to compile/run (F9 Key)
    * this project will run the indexer in the ide for debugging or building    

## Pre-Built

* check the releases tab in github for pre-built binaries to get started right away


**Tip Jar**
  * :dollar: BTC - bc1q55qh7xptfgkp087sfr5ppfkqe2jpaa59s8u2lz
  * :euro: LTC - LPbvTsFDZ6EdaLRhsvwbxcSfeUv1eZWGP6


cheers / kanpai 🍻
