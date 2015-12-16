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
我們可以像使用其它保護式表達式那樣使用它們。

























