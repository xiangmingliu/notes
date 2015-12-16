函數語法
模式匹配
=========

既然已經可以存儲和編譯我們的代碼了，那我們就可以開始編寫一些更高級的函數了。
到現在爲止我們寫的都是及其簡單的例子， 它們讓人感到乏味。讓我們將嘗試一些令
人興奮的東西。我們要寫的第一個函數根據性別向某人問好。在大多數語言中， 我們
是這麼寫的：

```
function greet(Gender, Name)
    if Gender == male then
        print("Hello, Mr. %s!", Name)
    else if Gender == female then
        print("Hello, Mrs. %s!", Name)
    else
        print("Hello, %s!", Name)
end
```
使用模式匹配， Erlang可以讓我們大幅度地減少代碼, Erlang的greet版本如下:

```
greet(male, Name) ->
    io:format("Hello, Mr. ~s!", [Name]);
greet(female, Name) ->
    io:format("Hello, Mrs. ~s!", [Name]);
greet(_, Name) ->
    io:format("Hello, ~s!", [Name]).
```
我承認Erlang的打印函數比其它語言的丑了一點， 但那不是關鍵點。這裏最大的不同是我們使用
模式匹配來定義需要使用的部分並且同時綁定我們需要的值. 沒有必要先綁定值再對它們進行比較！
所以， 我們沒有採用下面這種形式：

```
function(Args)
    if X then
        Expression
    else if Y then
        Expression
    else
        Expression
```
我們採用下面這種形式:

```
function(X) ->
    Expression;
function(Y) ->
    Expression;
function(_) ->
    Expression.
```
這兩種方式都能獲得相似的結果， 但是後面一種方式更具有聲明式的風格。每個這樣的函數聲明被
稱爲函數從句。函數從句必須以分號(;)分割， 它們一起形成了函數聲明。一個函數聲明被看作一
條大的語句， 這就是爲什麼最後一個函數從句要以英文的句號(.)結束。這種使用記號來決定工作
流的方式看起來很有趣，但是很快你就會適應它們了。你只能如此， 因爲再沒有其它的方式了。

```
註：`io:format`是通過將字符串中的佔位符替換成具體的值來達到格式化目的的。使用波浪號(~)
作爲前綴來表示一個佔位符。一些佔位符是內置的， 例如`~n`, 它將會被一個換行符替換。其它大部分
的佔位符都代表了格式化數據的一種方式。函數調用`io:format("~s!~n",["Hello"]).`, 包含了接受字符串
和位串作爲參數的佔位符`~s` 和~n。最終的輸出結果將會是"Hello!\n". 另外一個經常使用的佔位符是`~p`,
它會以一種更友好的方式打印Erlang的項(添加縮進和其它東西).

我們將在講到處理輸入輸出那章的時候再深入介紹`io:format`函數,但是現在你可以試試下面這些句子看看它們
的輸出結果有什麼不一樣：
`io:format("~s~n", [<<"Hello">>])`, `io:format("~p~n", [<<"Hello">>])`, `io:format("~~~n")`, 
`io:format("~f~n", [4.0])`, `io:format("~30f~n", [4.0])`. 這些是其中的一部分例子， 總之它有點
像其它語言中的`printf`. 如果你已經等不急想瞭解更多關於IO方面的內容， 你可以閱讀[在線文檔](http://erlang.org/doc/man/io.html#format-3)
```

函數中的模式匹配可能比上面那個例子更複雜。不知道你是否還記得在前面的章節中，我們可以使用模式匹配
從列表中獲取頭和尾。我們來試試！建一個新的`functions`模塊, 我們將會在裏面探索模式匹配：

```
-module(functions).
-compile(export_all). %%看在上帝的份上，請在後面將它替換爲 -export()
```
接下來我們要寫的第一個函數是`head/1`,它的功能和erlang:hd/1, 接受一個列表作爲參數並返回它的第一個元素。我們將
使用cons操作符來完成這個功能.

```
head([H|_]) -> H.
```
如果你在shell中輸入 `functions:head([1, 2, 3, 4]).`（首先先編譯代碼）, 它將返回 '1' 作爲結果, 如果我們想要獲取
列表的第二個元素， 可以編寫下面的函數
```
second([_, X|_]) -> X.
```
傳入的列表將會被Erlang進行解構並進行模式匹配。讓我們在shell裏面嘗試以下!

```
1> c(functions).
{ok, functions}
2> functions:head([1,2,3,4]).
1
3> functions:second([1,2,3,4]).
2
```
通過這一方法， 你可以獲取列表上任何一個元素， 但是對於上千個元素的列表來說這有點不切實際。
我們可以通過寫遞歸函數來完成這個任務， 我們後續會詳談這點。現在， 我們把注意力聚焦在模式
匹配上。我們在前面章節談到的自由變量和綁定變量在函數中同樣適用:我們可以利用這點來比較傳遞
給函數的兩個參數是否相同。讓我們創建一個接受兩個參數的`same/2`函數， 函數的功能是判斷它們
是否相等：

```
same(X, X) -> 
    true;
same(_, _) -> 
    false.
```

這看起來非常簡單。在解釋函數的工作原理之前， 我們先介紹下綁定和未綁定變量的概念,請看下面
這幅圖：
![](pictures/musical-chair.png)

