Core Stateful Data Structures
===


Summary
---

This is a data structures library which hold stateful data structures
and stateless data structures for Haskell. This can be used in a wide
variety of projects where are included Stack, Queue, Priority-Queue,
AVL Tree, Red-Black Tree among other data structures, with two variants,
stateless and stateful.


Installation
---

### What's in the Archive?

This archive contains a Haskell Cabal package, so you can download
it and install it using the cabal command.

### Install Instructions

```shell

tar xzvf csds.tar.gz
cd csds
cabal install

```

### Build (Only Build) And Test Instructions

```shell

tar xzvf csds.tar.gz
cd csds
cabal clean
cabal configure --enable-tests
cabal build
cabal test

```


Contribution
---
Contributions are always welcome, please follow these steps to submit your changes:

1. Install git from [http://git-scm.com/]()
2. Create a github account on [https://github.com]()
3. Set up your git ssh key using these instructions [http://help.github.com/set-up-git-redirect]()
4. Open the Apache Log Rev project home page on github on [https://github.com/dmw/CSDS.git]()
5. Click the "Fork" button, this will get you to a new page: your own copy of the code.
6. Copy the SSH URL at the top of the page and clone the repository on your local machine
7. The oficial "development" branch is "devel", so you should place your pull requests there.

```shell
git clone git@github.com:dmw/csds.git csds
```

7. Create a branch and switch to it

```shell
cd csds
git branch my-csds-branch
git checkout my-csds-branch
```

8. Apply your changes, then commit using a meaningful comment, that's the comment everybody will see!

```shell
git add .
git commit -m "Fixing issue 777, bleh bleh bender bleh"
```

9. Push the changes back to github (under a different branch, here myfeature-patch)

```shell
git push origin my-csds-branch
```

10. Open your forked repository on github at https://github.com/your-username/csds.git
11. Click "Switch Branches" and select your branch (my-csds-branch)
12. Click "Pull Request"
13. Submit your pull request to CSDS Developers


Support
---
We offer limited support at [http://coder.cl/products/csds/](http://coder.cl/products/csds/)


License
---
Licensed under the BSD3 License


Authors
---

Copyright(c) 2012 [Daniel Molina Wegener](https://github.com/dmw) [http://coder.cl/](http://coder.cl/)

