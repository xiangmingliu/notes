高階函數
=========

所有函數式編程語言中一個很重要的部分是能夠把你定義的函數作爲參數傳遞給另一個函數。
這會將你的函數綁定到一個變量上， 這樣我們就可以在函數中像處理其它變量那樣處理你的
函數， 我們稱可以接受其它函數作爲參數的函數爲高階函數， 高階函數是一種強大的抽象
工具， 它也是我們在Erlang中需要掌握的最好的工具。

再次， 這個概念也是源自數學， 主要源自lambda計算。我不會詳細的介紹lambda計算， 因爲很多人
在理解它上會有困難， 而且那也超出了本書的範圍。但是我會簡單地將它定義爲一個系統， 在這個
系統中， 所有的東西都是函數。甚至數字也是函數。因爲所有的東西都是函數，所以函數必須接受其它
函數作爲參數， 然後在函數中又使用其它函數來處理這個作爲參數的函數!

好吧, 這聽起來很怪異， 讓我們以一個例子來作爲開始吧:

```
-module(hhfuns).
-compile(export_all).

one() -> 1.
two() -> 2.

add(X,Y) -> X() + Y().
```
現在我們打開Erang shell，編譯模塊然後繼續：

```
1> c(hhfuns).
{ok, hhfuns}
2> hhfuns:add(one,two).
** exception error: bad function one
in function  hhfuns:add/2
3> hhfuns:add(1,2).
** exception error: bad function 1
in function  hhfuns:add/2
4> hhfuns:add(fun hhfuns:one/0, fun hhfuns:two/0).
3
```
是不是感到很困惑？ 如果我們知道它是怎麼工作的就不會如此困惑了， 在命令2中， 原子
`one` 和 `two`被傳遞給`add/2`, 這個函數中使用會將這兩個原子作爲函數名(`X() + Y()`).
如果函數名後沒有跟着參數列表，它就會被解釋爲原子，而原子不是函數， 所以我們的調用失敗了。
這就是爲什麼表達式3也失敗了：值1和2也不能作爲函數被調用， 我們需要的是函數！

這就是爲什麼需要在語言層面添加一個新的記號來讓我們能夠從外部模塊傳遞一個函數進來。
`fun Module:Function/Arity`幹的就是這個事：它告訴虛擬機使用那個函數， 並將它綁定到一個變量上。

這樣使用函數能給我們帶來什麼好處？ 我們展示一些例子來理解它。讓我們在`hhfuns`中添加一些函數，
這些函數遞歸一個列表， 並對元素加1或者減1:

```
increment([]) -> [];
increment([H|T]) -> [H+1|increment(T)].

decrement([]) -> [];
decrement([H|T]) -> [H-1|decrement(T)].
```
看到了嗎， 這些函數極其相似。基本上它們幹的是同一件事：循環整個列表， 對每個元素應用一個函數
(`+`或`-`), 然後再調用自身。這些代碼沒有太大的不同：只是所應用的函數和遞歸調用有所不一樣。在
一個列表上遞歸的核心是不變的。我們可以將相同的部分抽象到一個函數(`map/2`)中， 它將接受另一個
函數作爲參數：
```
map(_, []) -> [];
map(F, [H|T]) -> [F(H)|map(F,T)].

incr(X) -> X+1.
decr(X) -> X-1.
```

然後我們可以在shell中測試:
```
1> c(hhfuns).
{ok, hhfuns}
2> L = [1,2,3,4,5].
[1,2,3,4,5]
3> hhfuns:increment(L).
[2,3,4,5,6]
4> hhfuns:decrement(L).
[0,1,2,3,4]
5> hhfuns:map(fun hhfuns:incr/1, L).
[2,3,4,5,6]
6> hhfuns:map(fun hhfuns:decr/1, L).
[0,1,2,3,4]
```
我們得到的結果是一樣的， 但是我們已經創建了一個非常聰明的抽象！ 每次我們想對列表的每個元素應用一個函數的時候
我們就以這個函數爲參數調用`map/2`. 但是，每次我們想這麼使用的時候就得給這個函數取個名字，將其導出然後還要將
它編譯， 這有點煩人。 在平常使用中也不太實際。我們需要的是函數能夠在需要的時候聲明...

匿名函數
========

匿名函數， 或者fun， 就是用來解決上述問題的， 它允許我們通過內連的方式聲明一個特別的函數， 而不用給它命名。除了
遞歸調用它們自身外， 它可以做普通函數能做的一切， 它們的語法如下：

```
fun(Args1) ->
     Expression1, Exp2, ..., ExpN;
    (Args2) ->
     Expression1, Exp2, ..., ExpN;
    (Args3) ->
     Expression1, Exp2, ..., ExpN
end
```
可以通過以下方式來使用它們：

```
7> Fn = fun() -> a end.
#Fun<erl_eval.20.67289768>
8> Fn().
a
9> hhfuns:map(fun(X) -> X + 1 end, L).
[2,3,4,5,6]
10> hhfuns:map(fun(X) -> X - 1 end, L).
[0,1,2,3,4]
```
現在， 你已經看到函數式編程讓人着迷的地方：可以在底層代碼之上建立自己的抽象。
我們不用再考慮像循環這樣的邏輯， 因此可以將注意力集中在要幹什麼上而不是怎麼幹。

匿名函數對於這些抽象來說是極好的工具， 但它具備更多的隱藏威力：

```
11> PrepareAlarm = fun(Room) ->
11>                     io:format("Alarm set in ~s.~n",[Room]),
11>                     fun() -> io:format("Alarm tripped in ~s! Call Batman!~n",[Room]) end
11>                   end.
#Fun<erl_eval.20.67289768>
12> AlarmReady = PrepareAlarm("bathroom").
Alarm set in bathroom.
#Fun<erl_eval.6.13229925>
13> AlarmReady().
Alarm tripped in bathroom! Call Batman!
ok
```
勤務兵拿起電話！ 這裏發生了什麼事？ 好吧， 首先， 我們聲明了一個匿名函數， 並將它賦給PrepareAlarm.
這個函數此時並沒有運行起來：直到我們調用`PrepareAlarm("bathroom").`的時候它才運行。在那個點上，
`io:format/2`被執行並輸出 "Alarm set".然後第二個表達式(另一個匿名函數)被返回給調用者然後賦給 AlarmReady.
請注意在這裏Room的值是從父函數那獲得的（PrepareAlarm), 這個概念被稱爲閉包。

爲了理解閉包， 我們首先得理解作用域。 函數的作用域可以想象成所有變量和它們的值被存儲的地方。在函數
`base(A) -> B = A + 1.`中， A和B都是`base/1`作用域的一部分。 這就是說， 在`base/1`中的任何地方，我們
都可以訪問到A和B。當我說'任何時候'，我並不是開玩笑，孩子；這包括匿名函數在內：

```
base(A) ->
B = A + 1,
F = fun() -> A * B end,
F().
```
B和A仍然被綁定到`base/1`的作用域， 所以函數F依然可以訪問它們。這是因爲F繼承了`base/1`的作用域。就像現實
生活中的繼承那樣， 但是雙親不能獲得孩子所擁有的東西:

```
base(A) ->
    B = A + 1,
    F = fun() -> C = A * B end,
    F(),
    C.
```
在這個版本的函數中， B 仍然等於 `A+1`， F仍然能很好的執行. 但是，變量C只在匿名函數F的作用域中。當`base/1`在最後一行
嘗試訪問C的值， 結果它只會得到一個未綁定的變量。 如果你編譯這個函數， 編譯器將會拋出一個警告。繼承是單向的。

有一點很重要， 這種繼承關係會跟隨着匿名函數一起存在，而不管它走到哪裏，甚至當他被傳遞給另外一個函數的時候， 這種繼承
關係還存在着：

```
a() ->
Secret = "pony",
fun() -> Secret end.
 
b(F) ->
"a/0's password is "++F().
```
我們編譯並運行它：

```
14> c(hhfuns).
{ok, hhfuns}
15> hhfuns:b(hhfuns:a()).
"a/0's password is pony"
```
誰泄漏了`a/0`的密碼？ `a/0`自己。上面的匿名函數在聲明的有`a/0`的作用域，它在`b/1`中仍然挾帶
着這個作用域。這非常有用， 因爲我們可以將參數和內容帶離它原來的作用域，這樣我們就不再需要
整個的作用域了(就像我們在Batman的例子中那樣)。
當有若干個接受很多參數的函數時， 我們可以使用匿名函數在函數間維持狀態：

```
16> math:pow(5,2).
25.0
17> Base = 2.
2
18> PowerOfTwo = fun(X) -> math:pow(Base,X) end.
#Fun<erl_eval.6.13229925>
17> hhfuns:map(PowerOfTwo, [1,2,3,4]).
[2.0,4.0,8.0,16.0]
```
通過將`math:pow/2`封裝到一個匿名函數中，並將Base變量綁定到它的作用域中， 這樣我們就可以使用
列表中的整數對我們的base變量做指數運算。

編寫匿名函數時你可能會遇到的一個陷阱是重新定義作用域：

```
base() ->
A = 1,
(fun() -> A = 2 end)().
```

這將會聲明一個匿名函數然後立即運行它。因爲匿名函數繼承了`base/0`的作用域，並且使用`=`操作符比較
2和A(綁定了值1). 這肯定會失敗。但是， 如果在函數頭重新定義該變量是可能的：

```
base() ->
    A = 1,
    (fun(A) -> A = 2 end)(2).
```

這確實能正常工作。 如果你編譯它， 我們將會得到一個覆蓋警告("Warning: Variable 'A' shadowed in 'fun'").
覆蓋指的是定義了和父作用域中同名的變量。這是爲了防止一些錯誤，所以你應該考慮將你的變量重新命名。

```
更新：
從17.0開始， 語言開始支持帶有內部名字的匿名函數。是的，有名字的匿名函數。

但是這個名字只在函數的作用域內可見。這項技術的作用是可以定義匿名的遞歸函數：

18> f(PrepareAlarm), f(AlarmReady).
ok
19> PrepareAlarm = fun(Room) ->
19>    io:format("Alarm set in ~s.~n",[Room]),
19>     fun Loop() ->
19>        io:format("Alarm tripped in ~s! Call Batman!~n",[Room]),
19>        timer:sleep(500),
19>         Loop()
19>     end
19> end.
#Fun<erl_eval.6.71889879>
20> AlarmReady = PrepareAlarm("bathroom").
Alarm set in bathroom.
#Fun<erl_eval.44.71889879>
21> AlarmReady().
Alarm tripped in bathroom! Call Batman!
Alarm tripped in bathroom! Call Batman!
Alarm tripped in bathroom! Call Batman!
...

Loop變量引用了匿名函數本身，在它的作用域內， 可以像其它變量一樣被使用。（還有一句不知道怎麼翻譯).
```

現在我們會先把匿名函數放一邊， 我們繼續探索能夠避免寫遞歸函數的通用抽象。

映射，過濾器， 摺疊等等
=======================
在本章的開始， 我簡要地介紹了如果從兩個相似的函數中抽象出`map/2`函數。我也保証說這個函數對任何
類型的列表都適用： 我們回顧一下這個函數：

```
map(_, []) -> [];
map(F, [H|T]) -> [F(H)|map(F,T)].
```
但是我們還有其它類似的抽象。讓我們先來看看兩個函數：
```
%% only keep even numbers
even(L) -> lists:reverse(even(L,[])).
 
even([], Acc) -> Acc;
even([H|T], Acc) when H rem 2 == 0 ->
        even(T, [H|Acc]);
even([_|T], Acc) ->
        even(T, Acc).
 
%% only keep men older than 60
old_men(L) -> lists:reverse(old_men(L,[])).
 
old_men([], Acc) -> Acc;
old_men([Person = {male, Age}|People], Acc) when Age > 60 ->
        old_men(People, [Person|Acc]);
old_men([_|People], Acc) ->
        old_men(People, Acc).
```
第一個函數接受一個數字類型的列表並返回偶數列表。第二個函數遍歷一個以元組`{Gender, Age}`
爲元素的列表並返回那些年齡在六十歲以上的男人。這裏我們很難發現它們的共性， 但是我們找到
一些共同點。兩個函數都操作一個列表， 它們的目的也都是一樣的， 就是保留那些能夠通過測試的
元素， 拋棄那些沒有通過測試的函數。從這個一般化我們可以抽取出有用的信息並將它們抽象出來：

```
filter(Pred, L) -> lists:reverse(filter(Pred, L,[])).
 
filter(_, [], Acc) -> Acc;
filter(Pred, [H|T], Acc) ->
        case Pred(H) of
                true  -> filter(Pred, T, [H|Acc]);
                false -> filter(Pred, T, Acc)
        end.
```

爲了使用過濾函數， 我們只需要在外部完成測試即可。編譯`hhfuns`模塊，我們來試試這個函數：

```
1> c(hhfuns).
{ok, hhfuns}
2> Numbers = lists:seq(1,10).
[1,2,3,4,5,6,7,8,9,10]
3> hhfuns:filter(fun(X) -> X rem 2 == 0 end, Numbers).
[2,4,6,8,10]
4> People = [{male,45},{female,67},{male,66},{female,12},{unknown,174},{male,74}].
[{male,45},{female,67},{male,66},{female,12},{unknown,174},{male,74}]
5> hhfuns:filter(fun({Gender,Age}) -> Gender == male andalso Age > 60 end, People).
[{male,66},{male,74}]
``` 
這兩個例子演示了怎麼使用`filter/2`函數， 程序員只需要關心怎麼生成判定函數和列表。我們
不再需要考慮遍歷整個列表並把不想要的元素扔出去。將功能性的代碼抽象出來是一件很重要的事情：
嘗試分離那些總是保持不變的東西， 讓程序員提供那些變化的部分。

在前面的章節中， 在列表上進行的另一種遞歸操作是挨個查看每個元素並將它們歸納爲一個答案。這
被稱爲摺疊， 它可以應用在下列函數上：

```
%% find the maximum of a list
max([H|T]) -> max2(T, H).
 
max2([], Max) -> Max;
max2([H|T], Max) when H > Max -> max2(T, H);
max2([_|T], Max) -> max2(T, Max).
 
%% find the minimum of a list
min([H|T]) -> min2(T,H).
 
min2([], Min) -> Min;
min2([H|T], Min) when H < Min -> min2(T,H);
min2([_|T], Min) -> min2(T, Min).
 
%% sum of all the elements of a list
sum(L) -> sum(L,0).
 
sum([], Sum) -> Sum;
sum([H|T], Sum) -> sum(T, H+Sum).
```
爲了發現摺疊是怎麼工作的， 我們來看看上面這些操作之間的共同點和不同點。
就像前面提到的， 我們總是將一個列表縮減爲一個單一的值。因此，摺疊操作在遍歷整個列表的過程
中只保持一個值，不需要構建列表。然後我們需要將保護式抽離出去，因爲它們並不是共同的特性：
它們需要在用戶的函數中定義。基於這些考慮， 我們的摺疊函數看起來和sum函數很像。

在這3個函數中有一個微妙的元素我們沒有提及， 每個函數都需要有一個初始值。 在`sum/2`中， 我們使用
0來作爲初始值並執行`X = X + 0`， 如果我們做乘法那我們記得使用1來做初始值並執行`X = X * 1`,函數
`min/1`和 `max/1`看起來不太好確定初始值：如果列表只有負數，而我們使用1來做初始值， 那結果肯定就
不對了。所以我們使用列表的第一個元素來作初始值。很不幸， 我們並不是什麼時候都能確定初始值， 所以我們
把這個決定留給程序員。綜合以上考慮， 我們可以構建出下面的抽象：

```
fold(_, Start, []) -> Start;
fold(F, Start, [H|T]) -> fold(F, F(H,Start), T).
```
讓我們測試一下：
```
6> c(hhfuns).
{ok, hhfuns}
7> [H|T] = [1,7,3,5,9,0,2,3].   
[1,7,3,5,9,0,2,3]
8> hhfuns:fold(fun(A,B) when A > B -> A; (_,B) -> B end, H, T).
9
9> hhfuns:fold(fun(A,B) when A < B -> A; (_,B) -> B end, H, T).
0
10> hhfuns:fold(fun(A,B) -> A + B end, 0, lists:seq(1,6)).
21
```
你能想到的將一個列表縮減成一個元素都可以表達成摺疊操作。

有意思的是你可以將累積器表示成單個元素， 也可以將其表示爲一個列表。因此，我們可以使用
摺疊操作來構建一個列表。這意味着摺疊操作是個很普遍的概念， 我們可以使用摺疊來表達其它基於列表的
遞歸操作， 設置map和過濾器：

```
reverse(L) ->
        fold(fun(X,Acc) -> [X|Acc] end, [], L).
 
map2(F,L) ->
        reverse(fold(fun(X,Acc) -> [F(X)|Acc] end, [], L)).
 
filter2(Pred, L) ->
        F = fun(X,Acc) ->
                case Pred(X) of
                        true  -> [X|Acc];
                        false -> Acc
                end
        end,
        reverse(fold(F, [], L)).
```
它們都像之前那樣能正常工作。多麼強大的抽象?

映射, 過濾器和摺疊只是Erlang標準庫提供的很多抽象中的一部分， 請自行查閱(`lists:map/2`, `lists:filter/2`,
`lists:foldl/3`, `lists:foldr/3`),其它函數包括`all/2` 和 `any/2` 它們都測試一個列表， 並判斷列表中是所有
的元素都能通過， 還是至少一個元素能通過。我們可以使用`dropwhile/2`來查找列表中第一個滿足條件的元素，
`takewhile/2`執行相反的操作， 它會抽取所有不滿足條件的元素， 直到有一個滿足條件，`partition/2`則是上述兩
個元素的補充，它接受一個列表並返回兩個列表， 其中一個是滿足條件的元素， 另一個是不滿足條件的元素。我們經常
使用的列表函數包括:  `flatten/1`, `flatlength/1`, `flatmap/2`, `merge/1`, `nth/2`, `nthtail/2`, `split/2`等。

你會看到像zipper，unzipper， map連接和摺疊這樣的函數， 我建議你閱讀lists模塊的文檔， 你將會發現自己其實並不太需要
編寫遞歸函數。























