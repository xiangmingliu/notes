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
我們可以把這個音樂凳子的遊戲搬到Erlang的世界中， 我們會不斷嘗試坐到空椅子上。如果椅子已經
被其它人坐了， 那我們就不能坐到上面！ 讓我們來做個類比， 未綁定變量就是那些還沒有值綁定到
它們之上的變量（像遊戲中的空椅子）. 綁定一個變量就是將值綁到一個未綁定變量上。在Erlang中，
當你想要給一個綁定變量重新綁定一個值的時候， 除非新舊兩個值是相同的，否則將會發生錯誤。讓
我們考慮下圖片中右邊那條蛇：如果此時又來了一些蛇， 這並沒有改變遊戲的狀態， 只是你有了更多
的憤怒的蛇。如果此時另外一個動物坐到了凳子上(一只老實的獾)，事情將會變得糟糕。相同的值對
綁定變量來說沒啥問題， 給它綁定不同的值則會讓事情崩潰。如果你對這個概念還不清楚可以回頭閱讀
不可變的變量那一小節

回到我們的代碼：當我們調用same(a, a)的時候， Erlang虛擬機首先會看到第一個X是未綁定的：所以
它會將a綁定到它上面。然後虛擬機繼續處理第二個參數， 此時它發現X已經被綁定了。然後它會將第二
個傳入的參數值`a`和變量的值進行比較, 看它們是否匹配。結果發現是匹配的， 函數返回true。如果比較發現
兩個值不一樣， 這個分支的匹配將會失敗， VM會繼續匹配第二個函數從句，這個從句不關心傳入的參數(如果
已經是最後的分支了，那沒什麼可挑剔的)而直接返回false. 注意， 這個函數可以接受任何類型的參數，它對
所有類型的數據都能其作用，並不僅限與列表和整數變量。我們舉一個更高級點的例子， 下面這個函數在日期
合法的前提下將它打印出來：

```
valid_time({Date = {Y,M,D}, Time = {H, Min, S}}) ->
        io:format("The Date tuple (~p) says today is: ~p/~p/~p, ~n", [Y,M,D]),
        io:format("The time tuple (~p) indicates: ~p:~p:~p. ~n", [H,Min,S]);
valid_time(_) ->
        io:format("Stop feeding me wrong data!~n").
```

我們注意到在函數頭中也可以使用=操作符，這讓我們既可以匹配元組裏面的內容`({Y,M,D})`, 也可以把元組當成
一個整體(Date)進行匹配。我們可以使用下面的方式來對函數進行測試：

```
4> c(functions).
{ok, functions}
5> functions:valid_time({{2011,09,06},{09,04,43}}).
The Date tuple ({2011,9,6}) says today is: 2011/9/6,
The time tuple ({9,4,43}) indicates: 9:4:43.
ok
6> functions:valid_time({{2011,09,06},{09,04}}).
Stop feeding me wrong data!
ok
```
上面的例子還有一個問題！ 這個函數會接受任何東西作爲值， 甚至是文本或者原子，只要元祖的形式滿足
`{{A, B, C}, {D, E, F}}`的形式。這是模式匹配的一個缺陷：在模式匹配中可以使用像原子這樣很確定的
值，或者像代表列表的head|tail這樣的抽象值， N個元素的元組或其他任何東西(_和未綁定變量),等等。爲
了解決這個問題， 我們使用保護式.

保護式！保護式！
===============

保護式是函數頭中額外的從句， 用來增加模式匹配的表達能力。如上面提到的， 模式匹配是有侷限性的， 它無法
表達特定數據類型中某一範圍內的值。其中一個我們無法表達的概念是計數：這個12歲的籃球運動員身高是否達到
專業運動員的水平？ 這個距離對你來說是否太遠了？ 你是否到了可以領駕照的年齡？ 使用簡單的模式匹配我們無法
回答這些問題。我的意思是，你可能需要如下的方式來建模駕照的問題：

```
old_enough(0) -> false;
old_enough(1) -> false;
old_enough(2) -> false;
...
old_enough(14) -> false;
old_enough(15) -> false;
old_enough(_) -> true.
```
但是這看起來太不現實了。如果你想的話當然可以這麼幹， 但是你就等着自己維護自己的代碼吧。如果你想有一些
朋友的話，我們還是新建一個guards模塊來尋找駕照問題中的"正確"的解決方案吧：

```
old_enough(X) when X >= 16 -> true;
old_enough(_) ->false.
```
搞定！ 我們可以看到， 這看起來更簡潔。注意保護式表達式的基本規則是它們必須在成功的情況下返回`true`.如果
它返回了`false`或者拋出異常的話保護式將會失敗. 假設我們想禁止年齡超過104歲的老人駕車.現在合法的假設年齡
變成了從16歲到104歲。讓我們繼續完善我們的保護式從句：

```
right_age(X) -> when X >= 16, X=< 104 -> 
    true;
right_age(_) ->
    false.
```

保護式中的逗號(,)和操作符`andalso`類似而分號(;)和`orelse`類似. 上面的保護式中只有當
兩個表達式都成功整個保護式才能通過。我們也可以用相反的方式來表示函數:

```
wrong_age(X) -> when X < 16; X > 104 ->
        true;
wrong_age(_) ->
        false.
```
用這種方式我們也能得到相同的結果。請測試它們（我們應該總是進行測試！）。在保護式表達式中，
分號(;)的作用和orelse是一樣的：如果第一個表達式失敗，它將會嘗試第二個，然後是下一個，直到
有一個表達式成功或者所有的表達式都失敗。

我們也可以在函數中使用除了比較和布爾運算之外的其它功能， 包括數學運算(`A*B/C >= 0`)和判斷
數據類型的函數, 例如, `is_integer/1`, `is_atom/1`, 等等。（我們將在後續的章節回過頭來談論它們）.
保護式的一個缺點是處於防止副作用的考慮它們不接受用戶自定義函數. Erlang不是純函數式語言(Haskell是)
因爲它大量地依賴副作用：你可以做I/O, 在actor之間發送消息，或者在需要的時候拋出異常。不存在簡單的
方法來判斷你在保護式中使用的函數是否包含了打印文本的語句或通過很多個函數從句來捕獲重要的錯誤。因此，
Erlang選擇了不相信我們(也許這是正確的選擇).

那就是說， 你需要能夠理解保護式的基本語法， 以便在遇到的時候能夠理解它們。

```
註：我已經拿保護式中的`,`和`;`與操作符`andalso`和`orelse`做了類比. 但是它們並不完全相同。前面一對
將會捕獲異常而後一對則不會。這意味着在保護式`X >= N; N >= 0`,中， 如果第一個表達式發生了異常，第二
個表達式仍然被計算，整個保護式仍然可能會通過; 相反， 如果在表達式 `X >= N orelse N >= 0` 中,如果第
一個表達式發生異常， 第二個表達式將會被忽略，並且整個表達式都會失敗。

但是(總是會有一個'但是'), 在保護式中， 只有 `andalso`和`orelse`能被嵌套使用。就是說 
`(A orelse B) andalso C` 是合法的保護式， `(A;B), C` 卻不是。所以最好在需要的時候混合使用它們
```
If語句！？
============

`If`的作用和保護式的作用類似， 使用和保護式一樣的語法, 但是我們在函數頭之外使用它。事實上, `if`從句
被稱爲保護式模式。Erlang的`if`和其它語言中的`if`不太一樣； 和它們相比, Erlang中的`if`顯得有點異類，
如果給它取個別的名字， 也許現在人們對它的接受度還高一點。當跨進Erlang的王國， 你需要拋棄以往所有關於
`if`的認識。

爲了看清楚if表達式和保護式的相似性， 請看下面的例子：

```
-module(what_the_if).
-export([heh_fine/0]).

heh_fine() ->
        if 1 =:= 1 ->
                   works
        end,
        if 1 =:= 2; 1 =:= 1 ->
                   works
        end,
        if 1 =:= 2, 1 =:= 1 ->
                   fails
        end.
```
將代碼保存到文件`what_the_if.erl`中， 我們來試試看會發生什麼:
```
1> c(what_the_if).
./what_the_if.erl:12: Warning: no clause will ever match
./what_the_if.erl:12: Warning: the guard for this clause evaluates to 'false'
{ok,what_the_if}
2> what_the_if:heh_fine().
** exception error: no true branch found when evaluating an if expression
in function  what_the_if:heh_fine/0
```
啊，不！ 編譯器警告我們說第12行的if(`1=:=1, 1=:=2`)裏面沒有從句會被匹配因爲
唯一的從句返回了`false`. 請記住一點，Erlang中所有的東西都要有返回值，`if`表達式
也不例外。因此， 當Erlang不能找到一個成功的保護式時， 它崩潰了：它不直到該返回什麼。
因此， 我們需要添加一個匹配所有情況的分支。在大多數語言中， 我們會使用`else`來做這
件事。在Erlang中, 我們使用`true`(這就能解釋爲什麼虛擬機發狂而時候它拋出了'沒有找到true分支'):
```
oh_god(N) ->
    if N =:= 2 -> might_succeed;
       true -> always_does
    end
```
讓我們來測試一下這個新函數（讓那個舊的函數呆在那， 繼續發出警告，我們讓它呆在那給我們留個提醒）： 

```
3> c(what_the_if).
./what_the_if.erl:12: Warning: no clause will ever match
./what_the_if.erl:12: Warning: the guard for this clause evaluates to 'false'
{ok,what_the_if}
4> what_the_if:oh_god(2).
might_succeed
5> what_the_if:oh_god(3).
always_does
```

下面這個例子向我們展示怎麼在一個`if`表達式中使用多個保護式. 這個例子也演示了我們怎麼
利用`所有的表達式都必須返回東西`這一點：我們將`if`表達式的值綁定到Talk變量中，然後在
元組裏把它和一個字符串連接.當我們閱讀代碼的時候， 我們可以清晰地看到缺少`true`會讓事情
變得一團糟糕. 因爲Erlang中沒有null值(像lisp中的nill， C的NULL， Python中的None等):

```
%% note , this one would be better as a pattern match in function heads!
%% I'm doing it this way for the sake of the example.
help_me(Animal) ->
        Talk = if Animal == cat -> "meow";
                  Animal == beef -> "mooo";
                  Animal == dog -> "bark";
                  Animal == tree -> "bark";
                  true -> "fgdadfgna"
               end,
        {Animal, "says " ++ Talk ++ "!"}
```

讓我們來試一下：
```
6> c(what_the_if).
./what_the_if.erl:12: Warning: no clause will ever match
./what_the_if.erl:12: Warning: the guard for this clause evaluates to 'false'
{ok,what_the_if}
7> what_the_if:help_me(dog).
{dog,"says bark!"}
8> what_the_if:help_me("it hurts!").
{"it hurts!","says fgdadfgna!"}
```
你可能會像其他Erlang程序員那樣感到疑惑， 爲什麼不使用`else`而使用`true`來控制流程呢？
畢竟`else`更爲人們所熟知。Richard O'Keefe在Erlang的郵件列表中給出了下列的答案。我直接把
它引用過來了， 因爲找不出更好的答案：

```
我們對`else`確實更熟悉，但這並不意味着它試個好東西。我知道在Erlang中通過編寫';true->'很
容易達到`else`的效果， 但是我們幾十年的變成心理學告訴我們這不是一個好的主意。我已經開始尋找
其它方式：
                                    vs
if X > Y -> a()                                 if X > Y -> a()
  ; true -> b()                                  ; X =< Y -> b()
end                                             end
if X > Y -> a()                                 if X > Y -> a()
 ; X < Y -> b()                                  ; X < Y -> b()
 ; true  -> c()                                  ; X ==Y -> c()
end                                             end
我在編寫這些代碼的時候感到很惱火， 但這讀起來清晰多了。
```
應該避免使用`else`或`true`, ，當我們構造覆蓋所有邏輯情況
的分支而不是使用`catch all`分支, `if`更加容易閱讀


Case分支
===========

如果`if`表達式像一個保護式， 那麼`case ... of`則更像整個函數頭：我們可以對每個參數使用複雜
的模式匹配，而且能在參數之上構建保護式！

因爲我們對語法越來越熟悉， 所以已經不需要太多的例子。我們來寫一個應用到set(包含不重複值的集合)
上的append函數， 我們用無序列表來表示這個set。從效率上看， 這應該是最糟糕的實現，但是現在我們
關心的是語法：

```
insert(X,[]) ->
    [X];
insert(X,Set) ->
    case lists:member(X,Set) of
        true  -> Set;
        false -> [X|Set]
    end.
```
如果我們以一個空的set和X來調用這個函數， 它將返回一個包含X的list。否則
`lists:member/2`將會檢查元素是否是list的成員，如果是， 它將返回`true`,
反之返回`false`, 在這個例子中， 如果元素已經在list中了，我們就不會修改這個list，否則
我們將X添加爲list的第一個元素.

上面的例子中， 只是使用了簡單的模式匹配, 我們可以使用更複雜一點的：
```
beach(Temperature) ->
    case Temperature of
        {celsius, N} when N >= 20, N =< 45 ->
            'favorable';
        {kelvin, N} when N >= 293, N =< 318 ->
            'scientifically favorable';
        {fahrenheit, N} when N >= 68, N =< 113 ->
            'favorable in the US';
        _ ->
            'avoid beach'
    end.
```
這裏， “現在是否是去海濱遊玩”的答案使用3種不同的溫度系統給出：攝氏度，開爾文和華氏溫度。
我們還混合使用模式匹配和保護式以便能夠滿足所有的場景。就像前面指出的那樣， `case ... of` 表達式
更像是一堆帶保護式的函數頭。事實上， 我們可以使用以下方式來編寫我們的代碼：
```
beachf({celsius, N}) when N >= 20, N =< 45 ->
    'favorable';
...
beachf(_) ->
    'avoid beach'.
```
這就有一個問題了， 做條件判斷的時候什麼時候使用`if`, 什麼時候使用`case ... of`, 什麼時候使用函數.


該使用哪個？
===========

什麼情況下該使用哪個是個難以回答的問題。函數調用和`case ... of`之間的不同之處非常小：事實上，它們的
底層實現是一樣的， 使用哪個在性能上沒什麼差別。一個不同的地方在與當有多個參數的時候: `function(A, B) -> ... end.`
可以讓保護式和值去跟A和B進行匹配，但是我們卻需要像下面那樣case表達式:
```
case {A,B} of
    Pattern Guards -> ...
end.
```
這種方式很少見， 它很有可能會讓閱讀它的人感到吃驚。在這種場景中， 使用函數可能更加恰當。同樣，我們上面所寫的`insert/2`函數
比寫兩個函數來跟蹤簡單的`true`和`false`值更加簡潔。

另外一個問題是既然已經有了`case`和函數， 爲什麼還需使用`if`? `if`背後的道理很簡單: 它讓我們能夠使用不帶模式匹配的保護式。

當然， 上面說的這些都只是個人喜好， 對這些問題沒有一成不變的答案。這個話題在Erlang社區中仍然時不時地被拿出來討論。沒有人
會因爲你的選擇而視你爲異端， 只要它容易理解就可以了。就像Ward Cunningham所說的，“乾淨的代碼就是當你看到一段代碼的時候，
它就是你所期望的那樣子”

