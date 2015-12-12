启程
======

在Erlang中，你可以在模拟器中测试大部分的代码；它可以运行经过编译和部署之后
的脚本，但是它也支持在线编辑。在linux下通过`$ erl`运行Erlang shell。如果
正确的安装了环境，你将会看到以下输出：

```
Erlang/OTP 17 [erts-6.4.1.5] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V6.4.1.5  (abort with ^G)
1> 
```
恭喜， 你已经成功的运行了Erlang shell

对Windows用户来说，你也可以运行erl.exe shell， 但是推荐使用werl.exe，它位于开始菜单（程序 > Erlang）。
werl是Erlang shell的windows实现，它有自己的窗口和滚动条， 并且支持命令行编辑（例如拷贝-复制，这在windows
的cmd.exe shell中是一件非常痛苦的事）。如果你想重定向标准输入输出或者使用管道的话，还是需要erl shell的。

现在我们可以在模拟器中运行代码了，但是， 首先让我们先对它熟悉以下。

Shell 命令
======================

Erlang shell有一个基于Emacs子集的内置行编辑器。Emacs是个从70年代开始就非常流行的文本编辑器。如果你晓得Emacs，
这一切对你来说都不是问题。当然了， 这对其它人来说也不是什么大的问题。

首先，如果你输入了一些字符然后按下(Ctrl-A)，你会看到光标会移动到行首。(Ctrl-E)让光标移动到行尾。你可以
使用方向键来进行向前或者向后查找，查看上一行或者下一行命令。

如果你输入li并按tab键，shell会帮你补全成lists:。然后再按tab键，shell会给你提示可用的函数列表。你可能会发现
给出的信息看起来很奇怪，但是不要担心， 你很快就会熟悉它们。

我想我们已经知道了足够多的shell功能, 但是我们遗漏了一件事：我们还不知道怎么退出！ 有一个很便捷的方法。输入
`help()`。你将会看到一堆你可以在shell中使用的命令的相关信息(不要忘了(`.`)号因为shell看到它才会执行命令)。
后续我将会使用这其中的一些命令，但是现在我们需要关心的仅仅是下面这行
```
q() -- quit -shorthand for init:stop()
```
这是退出shell的一种方式(事实上， 是两种)。但是， 当shell发生冻结的时候这条命令就无能为力了！如果你细心观察，
当启动shell的时候， 有一小段文字 'aborting with '。让我们来试一下， 然后输入h获取帮助。

```
User switch command
 --> h
  c [nn]            - connect to job
  i [nn]            - interrupt job
  k [nn]            - kill job
  j                 - list all jobs
  s [shell]         - start local shell
  r [node [shell]]  - start remote shell
  q                 - quit erlang
  ? | h             - this message
 --> 
```
如果你输入i然后c，Erlang会停止当前正在运行的代码，然后把你待会一个可响应的shell中。j给你一个正在运行的进程
的列表(数字后面跟一个星号表明这是你当前正在运行的工作)，你可以输入i然后跟着这个数字来中断当前的工作。使用k
会将shell杀死而不是将其中断。输入s会新建一个shell。

```
Eshell V6.4.1.5  (abort with ^G)
1>"OH NO THIS SHELL IS UNRESPONSIVE!!! *hits ctrl+G*"
 User switch command
  --> k
  --> c
 Unknown job
  --> s
  --> j
    2* {shell,start,[]}
  --> c 2
Eshell V6.4.1.5  (abort with ^G)
1> "YESS!"
```



