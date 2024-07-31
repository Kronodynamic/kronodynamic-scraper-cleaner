# Crawler dump cleaner

## Dependencies

You need to install the last version of [RocksDB](https://github.com/facebook/rocksdb):


```sh
# Ubuntu install instructions
sudo apt-get install -y zlib1g-dev libbz2-dev liblz4-dev libsnappy-dev libzstd-dev libgflags2.2 libgflags-dev
cd /tmp
git clone git@github.com:facebook/rocksdb.git 
cd rocks_db
make shared_lib install
```
The code above will install RocksDB library at `/usr/local/lib`.

Then go to the crawler project root, and build it:
```sh
cd <project-root>
LD_LIBRARY_PATH=/usr/local/lib gerbil build
```

And then run it:
```sh
LD_LIBRARY_PATH=/usr/local/lib gerbil env kronodynamic-scraper-cleaner parse <directory>
```

It's advisable to add the path `/usr/local/lib` to global `LD_LIBRARY_PATH`.

