類型（或者缺乏類型）
動態強類型
===================

你可能已經注意到， 在輸入例子代碼的時候， 我們從來不需要指定變量或者函數的類型。
在進行模式匹配的時候， 我們的代碼不需要知道它要匹配的對象的類型， 元組`{X, Y}`可以
和`{atom, 123}`匹配， 也可以和`{"A string", <<"binary stuff">>}`, `{2.0, ["strings", "and", atoms]}`匹配

當無法完成匹配時， 它將拋一個錯誤到我們面前， 但只有當代碼運行的時候它才這麼幹。這是因爲Erlang是動態類型的:
每個錯誤都是在運行時才發生的，編譯器不會時不時地向我們拋出錯誤， 就像我們在第三章的那個例子: `"llama + 5"`

靜態類型和動態類型的支持者之間爭論的焦點和縮寫的軟件的安全有關。通常的看法認爲良好的靜態類型系統可以在運行代碼
之前捕獲大部分的錯誤。因此， 靜態類型語言通常會比動態類型語言安全。儘管這和其它動態類型語言相比可能是真的，但
Erlang有所不同， 有明顯的證據證明這點。最好的例子就是經常被提及的由愛立信AXD 301 ATM交換機提供的9個9
(99.9999999%)的可用性， 該系統由1百萬Erlang代碼組成。請注意這並不是說基於Erlang的系統中任何組件都不會失敗，只是說
那個通用交換機系統達到了99.9999999%的可用性， 包括停電計劃在內。這一部分是因爲Erlang中一個地方的錯誤不會影響到整個
系統。錯誤來自程序員，硬件失敗或者網絡失敗也被考慮在內：語言包含了允許我們將程序分佈到不同節點上， 處理非預期錯誤
和不間斷運行的特性。

簡而言之， 儘管大部分語言和類型系統爭取讓程序做到無錯， Erlang使用了另一種策略，它假設錯誤
總是會發生的並提供方式從錯誤場景中恢復：Erlang的動態類型系統並不障礙程序的可靠性和安全。這
聽起來就像其它預言一樣， 但是你會在後續章節看到我們是怎麼做到的。

```
註：選擇動態類型的歷史原因很簡單; 實現Erlang的那幫人大多來自動態類型語言領域， 因此，讓Erlang
使用動態類型對它們來說是很自然地選擇。
```
Erlang也是強類型的。弱類型語言會對項式做隱式類型轉換。 如果Erlang是弱類型的， 那 `6 = 5 + "1"`
就是合法的。但是， 它將會拋參數錯誤異常：

```
1> 6 + "1".
** exception error: bad argument in an arithmetic expression
in operator  +/2
called as 6 + "1"
```
當然， 有時候我們也需要對類型進行轉換：爲了存儲，將普通字符串轉換成位串或者將整型轉換成浮點型。Erlang
標準庫提供了大量的函數來完成這項工作。

類型轉換
=========

像其它許多語言一樣， Erlang通過將其重新塑造來將一個項式的類型變成另一種類型。這是通過內置函數來完成的，因爲
很多轉換不能使用Erlang本身來完成。這些函數的形式如：<type>_to_<type>, 它們都是在`erlang`模塊中實現的， 讓
我們來試試其中一些：

```
1> erlang:list_to_integer("54").
54
2> erlang:integer_to_list(54).
"54"
3> erlang:list_to_integer("54.32").
** exception error: bad argument
in function  list_to_integer/1
called as list_to_integer("54.32")
4> erlang:list_to_float("54.32").
54.32
5> erlang:atom_to_list(true).
"true"
6> erlang:list_to_bitstring("hi there").
<<"hi there">>
7> erlang:bitstring_to_list(<<"hi there">>).
"hi there"
```
當然了， 我們又遇到了Erlang的一個瑕疵：因爲使用了<type>_to_<type>這樣的模式，每次
要添加一個類型到語言中， 都要加一大堆的內置函數進行類型轉換！下面是完整的列表：

```
atom_to_binary/2, atom_to_list/1, binary_to_atom/2, binary_to_existing_atom/2, binary_to_list/1,
bitstring_to_list/1, binary_to_term/1, float_to_list/1, fun_to_list/1, integer_to_list/1, 
integer_to_list/2, iolist_to_binary/1, iolist_to_atom/1, list_to_atom/1, list_to_binary/1, 
list_to_bitstring/1, list_to_existing_atom/1, list_to_float/1, list_to_integer/2, list_to_pid/1, 
list_to_tuple/1, pid_to_list/1, port_to_list/1, ref_to_list/1, term_to_binary/1, term_to_binary/2,
tuple_to_list/1.
```

太多的類型轉換函數了， 我們將會在本書中遇到它們中的大部分， 但是實際上我們用不到它們的全部.

守衛數據類型
============
Erlang的基本數據類型很容易用肉眼區分: 元組使用花括號， 列表使用方括號， 字符串使用雙引號，等。
因此很容易使用模式匹配來強制使用某種數據類型:函數`head/1`只能接受一個列表,否則，和`(H|_)`進行
匹配時將會失敗。

但是， 我們在處理數字時會遇到一點麻煩， 因爲模式匹配不能指定範圍。因此，我們得在函數頭中使用保護式
來對溫度進， 合法駕駛年齡等進行限制。現在， 我們又遇到另一個路障。我們怎麼寫一個保護式來保証模式只
能匹配特定類型的數據，例如數字，原子或者位串？

Erlang爲這個任務提供了專門的函數。它們接受單個參數，並在參數是某一類型的數據時返回true，否則返回false。
它們是少數能在保護式表達式中使用的函數之一， 被稱爲類型測試內置函數：

```
is_atom/1           is_binary/1        
is_bitstring/1      is_boolean/1        is_builtin/3       
is_float/1          is_function/1       is_function/2      
is_integer/1        is_list/1           is_number/1        
is_pid/1            is_port/1           is_record/2        
is_record/3         is_reference/1      is_tuple/1    
```
我們可以像使用其它保護式表達式那樣使用它們。你可能會感到好奇， 沒有一個函數能夠計算出
給定變量的類型（`例如type_of(X) -> Type`）.答案很簡單。Erlang只針對正確的情況進行編程：
我們只針對我們知道將要發生的和我們所期望的情況編程。任何其它情況都會儘可能快地引發錯誤。
雖然這聽起來很愚蠢， 我們將會在錯誤和異常處理章節進行解釋， 希望到時你能清晰地理解這點。

```
註：允許使用在保護式表達式的函數中，類型測試內置函數佔了大半， 剩餘的也是內置函數，但它們
並不用於類型測試， 這些函數包括：
```
abs(Number), bit_size(Bitstring), byte_size(Bitstring), element(N, Tuple), float(Term), hd(List), 
length(List), node(), node(Pid|Ref|Port), round(Number), self(), size(Tuple|Bitstring), tl(List), 
trunc(Number), tuple_size(Tuple).
```

函數node/1和self/0和分佈式以及進程/actor有關。最後我們會使用到它們， 但在那之前我們還要介紹相關概念。
```
看起來Erlang的數據結構非常有限， 但是我們可以用元組和列表來構造其它複雜的數據結構，沒啥好擔心的。作爲
一個簡單的例子, 二叉樹的節點可以表示爲：`{node, Value, Left, Right}`, 這裏Left和Right或者是和這裏一樣
的節點， 或者是空元組。我也可以將自己表示爲:

```
{person, {name, <<"Fred T-H">>},
         {qualities, ["handsome", "smart", "honest", "objective"]},
         {faults, ["liar"]},
         {skills, ["programming", "bass guitar", "underwater breakdancing"]}}
```
這個例子說明元組和列表， 並給它們填充數據，我們就可以構造複雜的數據結構， 並建立處理它們的函數。
```
更新：
R13B04新加了內置函數`binary_to_term/2`, 該函數和`binary_to_term/1`都可以讓我們將數據反序列化，但
它允許我們通過第二個參數提供一些選項，如果你傳進`[safe]`, 當二進制數據中存在未知的原子或匿名函數時，
它將不會反序列化， 因爲這將會耗盡內存。
```
類型成癮者
============
這一小節是爲那些因爲種種原因脫離不了靜態類型系統的讀者。我們將會談論一些高級一點的理論，並不是所有的
讀者都能理解這些內容。我還將在這裏介紹Erlang中用於做靜態類型分析的工具， 那樣我們就可以安全地定義自己
的類型了。我們介紹這些工具是爲了讀者能在後續的章節中能夠理解它們。但是對於編寫可靠的Erlang程序它們不
是必須的。因爲我們後續還會介紹它們， 所以關於安裝運行它們的細節在這裏就先不介紹了。再次申明， 這一小結
是給那些無法在高級類型系統下工作的讀者準備的。

這些年， 很多人嘗試過在Erlang之上構建類型系統。其中的一次在1997年，由Simon Marlow主導， Glasgow Haskell
編譯器的主要開發者， 和Philip Wadler，負責Haskell的設計，對monads概念提供了理論支持, Joe Armstrong在後來的
論文中寫到：
```
一天Phil給我打電話然後說 a)Erlang需要一個類型系統, b)它已經寫了一個類型系統的原型, c)它由一年的公休並打算爲
Erlang寫一個類型系統， 它問我們對此是否感興趣？ 我們的回答是----"是的，感興趣"

Phi Wadler和Simon Marlow花了一年的時間來完成類型系統並將結果發表在[20]. 項目的結果由點讓人失望。剛開始，只能
對語言中的一個子集進行類型檢查， 最大的刪節是沒有對進程類型和進程經間的消息做類型檢查.
```
進程和消息是Erlang的核心特性， 這就能夠解釋爲什麼這個類型系統沒有被加到語言中。其它爲Erlang添加類型系統的努力
也失敗了。HiPE項目(爲了讓Erlang跑得更快)的結果產生了Dialyzer. 今天仍然在使用的靜態分析工具，它有自己的類型推斷
機制。

它所使用的類型系統基於成功類型， 一個不同於Hindley-Milner 和軟類型系統的概念。成功類型在概念上很簡單：類型推斷不
會嘗試爲所有的表達式都找出它們的類型， 但是它保証它推斷出來的類型都是正確的， 而它所發現的類型錯誤就真的是錯誤。

最好的例子就是函數`and`的實現， 它接受2個布爾值， 如果它們都是`true`就返回`true`， 否則返回`false`, 在Haskell的
類型系統中， 這寫作`and ::bool -> bool ->bool`, 如果在Erlang中實現它， 它應該長下面這樣：

```
and(false, _) -> false;
and(_, false) -> false;
and(true,true) -> true.
```
在成功類型系統中， 從函數推導出來的類型應該是 `and(_, _) -> bool()`, _代表任何東西.這很好解釋， 當我們在Erlang中
以`false`和`42`來調用這個函數時， 結果將會是`false`. 參數中只要存在一個是false， 就一定可以成功調用該函數. 如果你在
非Erlang環境中這麼調用函數的話， ML類型可能會拋出一個錯誤，並提示你修改（這常常會把用戶嚇一跳），如果你閱讀了論文
[implementation of success types](http://www.it.uu.se/research/group/hipe/papers/succ_types.pdf)的話可能會對這一點
更加清晰。我鼓勵任何類型重度患者都去閱讀一下這篇論文， 它是一種很有趣而又具有現實意義的定義。

類型定義和函數註解在Erlang增強計劃8([EEP 8](http://www.erlang.org/eeps/eep-0008.html)) 中詳細描述。如果你希望在Erlang中
使用成功類型，請學習 [TypEr application](http://user.it.uu.se/~tobiasl/publications/typer.pdf) 和Dialyzer, 它們都包含在標準
版本中。如果想使用它們, 請輸入`$ typer --help` 和 `$ dialyzer --help`， Windows用戶請輸入`typer.exe --help`和`dialyzer.exe --help`.

TypEr 用來給函數產生類型註解。 當應用到這個小的[FIFO實現](http://learnyousomeerlang.com/static/erlang/fifo.erl)上時， 它產生下列的
類型註解：
```
%% File: fifo.erl
%% --------------
-spec new() -> {'fifo',[],[]}.
-spec push({'fifo',_,_},_) -> {'fifo',nonempty_maybe_improper_list(),_}.
-spec pop({'fifo',_,maybe_improper_list()}) -> {_,{'fifo',_,_}}.
-spec empty({'fifo',_,_}) -> bool().
```

它推導出的結果相當正確。應該避免非正規列表因爲`lists:reverse/1`不支持它們...


註： 還有部分未翻譯

















