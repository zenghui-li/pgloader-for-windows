# pgloader-for-windows
Build on Windows 11 with pgloader 3.6.9 release version for Migrate from MySQL to PostgreSQL

You can follow my instructions below to build other [pgloader](https://github.com/dimitri/pgloader/releases) versions.

This is the [pgloader's Introduction](https://pgloader.io/).

## First
```
git clone https://github.com/dimitri/pgloader.git
```

Before you build pgloader,you need to prepare the following environment on your windows
- [`sqlite-dll-win-x86-3470200.zip`](https://www.sqlite.org/download.html),after you download it,copy `sqlite3.dll` to your pgloader directory
- If your windows does not have [`OpenSSL`](https://slproweb.com/products/Win32OpenSSL.html) 3.x(Required),Please select [`Win32 OpenSSL v3.4.0`](https://slproweb.com/download/Win32OpenSSL_Light-3_4_0.msi)(must) to download and install,after install，add it to your system's environment variables
- you also need `sybdb.dll`,download the [`FreeTDS binary`](https://appveyor-artifacts-enam.7b474ce6bd9813bd1e711f7cdc19151f.r2.cloudflarestorage.com/freetds-24821/freetds/3203/pjr92uw497iu2go1/vs2017_32-master.zip?X-Amz-Expires=300&X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Credential=23e97e8f2c472db9a7966a0f1cd5e0c6/20241219/us-east-1/s3/aws4_request&X-Amz-Date=20241219T123943Z&X-Amz-SignedHeaders=host&X-Amz-Signature=5d96f4de16e5852d5444e83e238751c79bdb3b9e3f8fadb9c908de4c953a97ae) file or bulid the [`FreeTDS`](https://www.freetds.org/docs.html) by yourself.And then you will get `sybdb.dll`,add the path where `sybdb.dll` is located to the system environment variables，if you download the FreeTDS binary, the path to `sybdb.dll` is `...\vs2017_32-master\bin`
- And then you need [`make`](https://www.msys2.org/) to build,download the installer: [`msys2-x86_64-20241208.exe`](https://objects.githubusercontent.com/github-production-release-asset-2e65be/80988227/cedfe079-9fca-44f4-b2bf-021f8f8107e2?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Credential=releaseassetproduction%2F20241219%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Date=20241219T133256Z&X-Amz-Expires=300&X-Amz-Signature=466675e3f5b5aef1a54d3de0ad9ed231148cea1c0b1ec04d075be51336deae68&X-Amz-SignedHeaders=host&response-content-disposition=attachment%3B%20filename%3Dmsys2-x86_64-20241208.exe&response-content-type=application%2Foctet-stream) after install，add it to your system's environment variables.
- pgloader is written in Common Lisp, so you need to install [`SBCL`](https://cyfuture.dl.sourceforge.net/project/sbcl/sbcl/2.3.2/sbcl-2.3.2-x86-windows-binary.msi?viasf=1) (Common Lisp Compiler) to compile it.After install，add it to your system's environment variables.

If you have installed the above tools but the terminal cannot recognize them, reopen the terminal or restart

## Next we will build
  ```
  cd pgloader
  make
  ```
then you will see `pgloader.exe` in `\pgloader-3.6.9\build\bin`,enjoy
