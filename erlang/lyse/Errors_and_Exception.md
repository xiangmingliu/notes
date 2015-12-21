錯誤和異常
不要跑那麼快
==============

沒有其它合適的地方來放置這一章的內容。現在， 我們已經學了足夠多的內容， 所以我們
可能會遇到各種錯誤，但是卻還沒有足夠的知識來處理它們。 事實上， 這一章並不能覆蓋
所有的錯誤處理機制。這是因爲Erlang有着兩種不同的範式：函數式和併發。函數式子集是
我從本書開始就一直在介紹的：引用透明性， 遞歸， 高階函數等。併發子集是讓Erlang
出名的原因：actor， 成千上萬的併發進程，監督樹等。

因爲我覺得函數式編程部分是併發部分的基礎， 所以這章我們只會涉及到函數式子集。如果我們相
管理錯誤， 我們首先得理解它們。

```
註： 雖然Erlang在函數式代碼中包含錯誤處理方法， 但是大多數時候， 我們被告知的確實任其崩潰。
我在 “介紹” 一章中暗示了這點。我們是在語言的併發部分使用這個機制的。
```

編譯錯誤
==========

我們平時說的錯誤有很多類型： 編譯期錯誤， 邏輯錯誤，運行期錯誤和用戶錯誤（generated errors）。
這一小節中我會集中關注編譯期錯誤

編譯期錯誤通常都是語法錯誤：檢查你的函數名， 語言的token(大括號， 括號， 點號，逗號)，函數的元
等。下面是常見的編譯期錯誤和解決方法：

module.beam: Module name 'madule' does not match file name 'module'
模塊名和文件名不一樣。

./module.erl:2: Warning: function some_function/0 is unused
你沒有將函數導出， 或者使用它的地方名字拼寫錯了或元數錯了。也可能是你寫了一個函數， 但已經不再需要它了，
請檢查你的代碼！

./module.erl:2: function some_function/1 undefined
函數不存在。很可能是聲明函數或者在`-export`屬性里將函數名字拼錯了。當函數無法編譯的時候也會拋出這個錯誤，
很可能是語法錯誤， 例如忘記以點號來結束一個函數。

./module.erl:5: syntax error before: 'SomeCharacterOrWord'
導致這個錯誤的原因很多， 沒有關閉的括號， 元組或者錯誤的結束了表達式(以一個逗號關閉case的最後一個分支)
其它的原因包括在代碼中使用了保留的原子，或者編解碼unicode的時候出錯了。

./module.erl:5: syntax error before: 
好吧， 這個錯誤提示看起來很不那麼明顯！這通常是一行沒有被正確地結束。這是上一種情況的特例，請留心它！

./module.erl:5: Warning: this expression will fail with a 'badarith' exception
Erlang是動態類型， 但是請記住它是強類型。編譯器會聰明地發現將會錯誤的表達式（例如， llama + 5). 但是複雜
的類型錯誤它是發現不了的。

./module.erl:5: Warning: variable 'Var' is unused
變量被聲明但是沒有被使用， 這通常是一個bug， 請仔細檢查這類錯誤。如果真的不打算使用變量， 請在它前面加下劃線。
(_Var).

./module.erl:5: Warning: a term is constructed, but never used
這個錯誤是因爲， 我們構造了一個列表， 聲明了一個元組或者匿名函數， 但是沒有將它們綁定到一個變量上，或者運行它們。
這個錯誤告訴我們我們乾了一些沒用的事， 或者我們錯誤的輸入了一些東西。

./module.erl:5: head mismatch
很可能我們的函數有多個頭部， 但是每個都有不同的元數。別忘記不同的元數代表不同的函數。

./module.erl:5: Warning: this clause cannot match because a previous clause at line 4 always matches
這是因爲我們在一個能匹配任何情況的分支後又定義了一個分支

./module.erl:9: variable 'A' unsafe in 'case' (line 5)
這是因爲我們可能在分支`case ... of`中定義了一個變量， 但是卻在外面使用了它。如果你相使用這類變量， 最好這麼
MyVar= `case ... of `使用它。

上面這些錯誤基本覆蓋了全部的編譯期錯誤。

邏輯錯誤!
==========

邏輯錯誤是最難查找和調試的錯誤。它們大多數來自程序員：if分支和case分支沒有考慮到所有的情況。將乘法和除法混合使用等
它們不會讓程序崩潰， 但是會給我們奇怪的答案。

碰到這種情況你就得自己解決它們了， 但是Erlang給我們提供了很多輔助工具幫助我們排查這類錯誤， TypEr和Dialyzer， 調試器和
tracing 模塊。對代碼進行測試是最好的預防措施。

運行期錯誤
==========

運行期錯誤具有破壞性， 因爲它會讓我們的代碼崩潰。雖然那Erlang總是幫助我們處理這些錯誤， 但是認識它們對我們還是有幫助的。

函數從句
```
1> lists:sort([3,2,1]).
[1,2,3]
2> lists:sort(fffffff).
** exception error: no function clause matching lists:sort(fffffff)
```
所有的函數分支都失敗了， 沒有函數分支能匹配到這個參數

case分支
```
3> case "Unexpected Value" of
3>    expected_value -> ok;
3>    other_expected_value -> 'also ok'
3> end.
** exception error: no case clause matching "Unexpected Value"
```
case分支中遺漏了一種情況， 或者調用者傳進了錯誤的數據，或者我們需要添加一個匹配任何一種情況的分支。

if_clause
```
4> if 2 > 4 -> ok;
4>    0 > 1 -> ok
4> end.
** exception error: no true branch found when evaluating an if expression
```
這和case分支很類似

badmatch
```
5> [X,Y] = {4,5}.
** exception error: no match of right hand side value {4,5}
```
模式匹配失敗

badarg
```
6> erlang:binary_to_list("heh, already a list").
** exception error: bad argument
in function  binary_to_list/1
called as binary_to_list("heh, already a list")
```
這和函數分支不匹配很類似， 切別在於它是由函數的編寫者進行參數檢查之後拋出的

undef
```
7> lists:random([1,2,3]).
** exception error: undefined function lists:random/1
```
函數沒有定義， 找不到函數

badarith
```
8> 5 + llama.
** exception error: bad argument in an arithmetic expression
in operator  +/2
called as 5 + llama
```
執行了不存在的算術運算

badfun
```
9> hhfuns:add(one,two).
** exception error: bad function one
in function  hhfuns:add/2
```
把變量當成函數使用了

badarity
```
10> F = fun(_) -> ok end.
#Fun<erl_eval.6.13229925>
11> F(a,b).
** exception error: interpreted function with arity 1 called with two arguments
```
調用函數的時候參數個數不正確

system_limit
進程數超系統限制了， 原子太長， 給函數的參數太多了， 原子的數目太大了， 連接的節點太多了等。

拋出異常
=========
在監視代碼的執行或者防止邏輯錯誤的時候， 引發一個運行期崩潰以便及早的發現問題是個不錯的主意。

Erlang中有3類異常： error, throw和exit。它們適用於撥通的使用場景。

錯誤
調用 `erlang:error(Reason)`會終止當前進程的執行並且包含一個調用棧跟蹤。就是這些錯誤引發上述運行期

當我們不能期望調用函數能夠處理當前的錯誤情況時就會拋出一個錯誤。 在遞歸那章中我們的樹查找操作就不
應該拋出一個錯誤，因爲當找不到鍵對應的值時， 我們期望調用者能處理這情況。

我們可以定義自己的錯誤：
```
1> erlang:error(badarith).
** exception error: bad argument in an arithmetic expression
2> erlang:error(custom_error).
** exception error: custom_error
```

退出
有兩種類型的退出， 內部退出和外部退出。通過執行`exit/1`進行內部退出,此時將會終止當前進程， 執行`exit/2`進行外部退出
此時， 將會終止其它進程

內部退出和錯誤很類似， 事實上， 以前它們是一樣的。現在， 退出不包括完整的調用棧。一般用它來處理進程間的錯誤。處理那些
會擴散到其它進程的錯誤消息

throw
throw是用來給程序員用的， 可以讓程序員檢查各種條件， 也可以用於非局部返回。






