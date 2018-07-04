この記事は[言語実装 Advent Calendar 2017](https://qiita.com/advent-calendar/2017/lang_dev)の18日目の記事です。 ~~なお、これを書いているのは1月末で、実に1ヶ月以上の滞納です。~~ 現在7月ですが。

# 異世界で使われている設定のアセンブリ言語をHaskellで実装する


[「異世界転生したけど日本語が通じなかった」](https://kakuyomu.jp/works/1177354054883808252)とかに出てくる異世界ファイクレオネで使われている設定のアセンブリ言語2003lkを、Haskellを使って実装した話を書いていく。

## 構成

アセンブリ言語でありながら、作ったのはアセンブラではなくインタプリタだったりする。というのも、これを作り始めた当時はまだ機械語の仕様が決まっておらず、アセンブラが作れなかったからである。2018年の目標はアセンブラを作ることである。

|ファイル名 |内容 |依存|
|:--- |:--- |:--- |
| [Memory.hs](https://github.com/jurliyuuri/OS/blob/master/assembler/Memory.hs) | 「メモリ」を定義する | なし |
| [Types.hs](https://github.com/jurliyuuri/OS/blob/master/assembler/Types.hs) | 他ファイルで使う型を定義する | Memory |
| [Messages.hs](https://github.com/jurliyuuri/OS/blob/master/assembler/Messages.hs) | エラーメッセージ。現状は英語しか出ないが、いつか日本語とかリパライン語にも対応したい | Types |
| [Parse.hs](https://github.com/jurliyuuri/OS/blob/master/assembler/Parse.hs) | パーサ（字句解析・構文解析・意味解析） | Types |
| [TentativeLoad.hs](https://github.com/jurliyuuri/OS/blob/master/assembler/TentativeLoad.hs) | 「メモリ」（ただしMemory.hsとは異なる）に命令を「ロード」する | Types |
| [Linker.hs](https://github.com/jurliyuuri/OS/blob/master/assembler/Linker.hs) | リンカ「のようなもの」 | Types, Parse, TentativeLoad |
| [Execute.hs](https://github.com/jurliyuuri/OS/blob/master/assembler/Execute.hs) | インタプリタ | Types, Memory, Linker |
| [CommonIO.hs](https://github.com/jurliyuuri/OS/blob/master/assembler/CommonIO.hs) | IO絡みの処理のうち、これから作る予定のアセンブラにも流用できそうな処理 | Parse, Messages, Types |
| [Main](https://github.com/jurliyuuri/OS/blob/master/assembler/xarzniar.hs) | パーサ・インタプリタ・IOをまとめてMainとする | Parse, Execute, Linker, Messages, Types, CommonIO |



## 言語仕様を軽く解説
詳しい説明は[設定一覧](http://jurliyuuri.com/OS/settings.html)に書いてあるので、以下言語実装の説明に必要最低限な要素だけ解説する。

### レジスタ
`f0`, `f1`, `f2`, `f3`, `f5`, `xx`が用意されている。

### メモリアクセス

| 構文 | 意味 |
| :-- | :-- |
|レジスタ名 `@`| レジスタに入っている番地のメモリを表す。|
|レジスタ名 `+` 定数`@`| レジスタに入っている番地に定数を足した番地のメモリを表す。|
|レジスタ名 `+` レジスタ名`@`|2つのレジスタに入っている数値を足した番地のメモリを表す。|

### `kue`と`xok`
`kue`<sup>[1](#myfootnote1)</sup>がラベルをexportする擬似命令で、`xok`<sup>[2](#myfootnote2)</sup>がラベルをimportする擬似命令である。現状、ファイル分割でできるのはラベルの共有だけなので、逆に`kue`のないファイルの先頭をエントリーポイントとする仕様となっている。


## パーサ

`a=(1+2)*3` みたいな再帰的な構文はないので、わざわざParsecを使うのもオーバーキルかなぁという考えのもと自前実装。コードは[Parse.hs](https://github.com/jurliyuuri/OS/blob/master/assembler/Parse.hs)。とはいえ単純な仕様なので、一気に字句解析・構文解析・意味解析して`Either ParseError ParsedFile`を吐く設計。`ParsedFile`型は

```
type ParsedFile = ([(Instruction, [Label])],([Label],[Label]))
```

と定義されている。最初の`[(Instruction, [Label])]`は「命令、およびそれに付随しているラベル」のリストであり、もう片方の`([Label],[Label])`は`kue`の一覧と`xok`の一覧を格納している。

### 字句解析
Haskellには`words`とかいう便利な関数があるので、それを使ってスペースで区切るだけである。ただし、2003lkの仕様上`+`と`@`の前後にはスペースを要求しないので、そこは`concatMap`を上手く使って処理している。

`words`処理をした後は、`beautify`という関数で「A `@`」または「A `+` B `@`」というパターンを見つけ、一つの『トークン』としてまとめている。不適切な位置に出てきた `+` や `@` はこの段階で弾いている。

### 構文解析・意味解析
`beautify`されたトークン列は`toInstructions`に渡される。`toInstructions`の処理の本体は`toI`関数である。
リストの先頭に命令があるので、それに従って引数を取得していく。なお、2003lkは命令からオペランド数が一意に定まる仕様となっているので、命令と命令の間に改行を置く必要はない。逆に、構文解析していく際にも行については一切考慮することなく先頭から読んでいっている。

それぞれの命令がどのような引数を取るのかも予め分かっているため、このタイミングで「即値への代入」などの誤った命令も弾いている。

## 「メモリ」
速度はそんなに求めてない（インタプリタですしおすし）ので、32ビット整数をキーとして8ビット整数を値とした連想配列で管理。「せっかくこんな富豪的プログラミングをしているのだから、それなりに機能性が欲しい」と思ったので、初期化していないメモリから読み取ろうとすると（そんなのは間違いなくバグなので）そのことがデバッグログに記録されるようにした。

## 「ロード」
パーサで処理した命令はメモリにロードされるはずなのだが、実装当時は機械語が定まっておらず、また`xok`や`kue`で複数ファイルを扱う機能もなかった。（この名残として、`toTentativeLoad`は`ParsedFile`ではなくその第一要素である`[(Instruction, [Label])]`を受け取る。）ということで、それぞれの命令の長さを擬似乱数で1〜4バイトと決めつけ、引数で与えられた初期アドレスの後ろにズラッと仮のアドレスを用意し、それを各命令に振るという実装とした。インタプリタなのだから動けばいいのである。

各ラベルの具体的なアドレスが定まるのもこの段階である。単一ファイル内に同一名のラベルが複数ある場合はここでエラーになるが、エラーの分類としては`LinkError`としている。

また、後に述べる「リンカ『のようなもの』」の都合上、単一のファイルでは65536バイト（`maxSize`定数で定義）の命令しか入れられないようになっている。

- 誰もまだそこまでのサイズのプログラムを2003lkで書いていない
- どうしても必要なら、`maxSize`を書き換えてインタプリタを再コンパイルすればいい
- 2003lkの制約ではなく、単にこのインタプリタ（と私のクソ実装）の制約に過ぎない

という理由で、「まあ許容可能でしょ」とした。
「ロード」の結果できあがるのは

```haskell
data TentativeLoad = TentativeLoad {
 tentativeAddressTable :: Data.Map.Map Word32 (Word32, Instruction), 
 labelTable :: M.Map Label Word32
 } deriving(Show, Eq, Ord)
```

という型である。`tentativeAddressTable`のキーである`Word32`はその命令の配置されているアドレス、その後ろの`(Word32, Instruction)`は「直後の命令のアドレス」および「命令本体」である。`labelTable`はラベル名からアドレスを引くための`Map`である。

## リンカ「のようなもの」
複数ファイルが扱えるようになったのは一旦実装を組み終わった後なので、既に出来上がっていた実装にねじ込む形でリンカ「のようなもの」を実装した。その結果、なかなか妙なことになってしまっている。

`linker`は`[ParsedFile]`を受け取る。`[ParsedFile]`の中で`kue`擬似命令を欠いているファイルはエントリーポイントであるので、それに0番、それ以外には正の整数を振る。もちろん、エントリーポイントが無かったりエントリーポイントが複数あったりしたらその時点で即`LinkError`である。

整数（`PageId`と呼ぶ）を振られた各`ParsedFile`は`dat :: Data.Map.Map Int ParsedFile`となっており、ここに`Data.Map.traverseWithKey`を適用して`dat`の各要素に`loadWithInt`関数を適用する。

`loadWithInt`関数は`n :: Int`と`(ils, (kues, xoks)) :: ParsedFile`を取って、まず`initialAddress + fromIntegral n * maxSize`の位置<sup>[3](#myfootnote3)</sup>に`ils`を`toTentativeLoad`する。それによってファイル内のlabelTableが作られるので、それが「`kues`にあるラベルはファイル内にあるはずだ」「`xoks`にあるラベルはファイル内にはあってはいけない」の2条件を満たしているか確認する。

最後に、複数のファイルが同一名のラベルを`kue`していないことを検証したら、`linker`の仕事は終わりである。

## インタプリタ

nヶ月ブランクが空いたので完全に忘れているんだけど、[Execute.hs](https://github.com/jurliyuuri/OS/blob/master/assembler/Execute.hs)の内容書けば多分良いんよね？やっていく。

### 概要

`Execute`モジュールからexportされている関数および定数は`initialHardware`, 
`execute`, `unwrapWith`, `execOne`の4つである。また、`CPU`が型だけ公開されており、`(CPU, Memory)`に`Hardware`という別名を付けている。

`CPU`は32ビットのフィールド`f0`・`f1`・`f2`・`f3`・`f5`・`nx`・`xx`と`Bool`型の`flag`を持つ。

さて、プログラムを実行するには、「プログラムを読み」「`Hardware`を読み書きし」「エラーが出たらそこで停止する」必要がある。さらに、現状では（多分）実装されていないが、ログを出力できるようにもしたい。ということで、以上のを表現するために、モナド変換子というものをフル活用して

```haskell
type VIO a = ReaderT Program (StateT Hardware (ExceptT Error (Writer Logs))) a
```

と定義してやる。`Program`はLinker.hsで定義されていて、

```haskell
data Program = Program {
 loads :: M.Map PageId (TentativeLoad, ([Label], [Label])), 
 kueTable :: M.Map Label PageId}
```

となっている。`Logs`はただの`[String]`である。`Error`はここでは`RuntimeError`の別名である。<sup>[4](#myfootnote4)</sup>

### 読んでいく

では、exportされている関数・定数を見ていこう。`initialHardware`は引数`initNX`を受け取ることで、以下のような`Hardware`を返す関数である。

- 「CPU」: `f0`・`f1`・`f2`・`f3`・`xx`がゴミで埋まっていて`flag`が`False`、`nx`に`initNX`が入っていて`f5`に`initialF5`<sup>[5](#myfootnote5)</sup>が入っている「CPU」  
- 「メモリ」: アドレス`initialF5`に値`outermostRetAddress`が書き込まれている以外には書き込みがされていない「メモリ」

次は`execOne`を見ていきたいところだが、そのためには先に`executeInstruction`を見ていく必要がある。この手のコードを書いた人なら一度は書いたことがあるであろう巨大switch - caseが`executeInstruction`である。この内部で重要な役割を果たすのは、右辺値から値を読む`getValueFromR :: Rvalue -> VIO Word32`と、左辺値に値を書く`setValueToL :: Lvalue -> Word32 -> VIO ()`である。

### 右辺値・左辺値の扱い、そしてラベルの名前解決

`Rvalue`と`Lvalue`は、それぞれ[Types.hs](https://github.com/jurliyuuri/OS/blob/master/assembler/Types.hs)で

```haskell
data Lvalue = Re Register | RPlusNum Register Word32 | RPlusR Register Register deriving (Show, Eq, Ord)
data Rvalue = L Lvalue | Pure Word32 | Lab Label deriving (Show, Eq, Ord)
```

と定義されている。それぞれ「左辺値とは、レジスタか、『レジスタに入っている番地に定数を足した番地のメモリ』か、『2つのレジスタに入っている数値を足した番地のメモリ』である」<sup>[6](#myfootnote6)</sup>「右辺値とは、左辺値に加え、純粋な32ビット整数か、ラベル（現状では真のリンカがないので、実行時に名前解決して番地を獲得する）」ということである。

`getValueFromR :: Rvalue -> VIO Word32`と`setValueToL :: Lvalue -> Word32 -> VIO ()`の実装は、基本的には`Hardware`をいじっているだけだが、実行時名前解決だけは説明が必要なので書く。<sup>[7](#myfootnote7)</sup>

現在このインタプリタでは名前空間を正しく実装するのをサボっているが、しかしながら別のファイルで定義されている同名のラベルは（`kue`されていなければ）衝突を起こさず実行できる。どのようなトリックを使っているかというと、前述した制約「単一のファイルでは65536バイト（`maxSize`定数で定義）の命令しか入れられないようになっている。」である。

各ファイルごとに`maxSize`の大きさのアドレス空間（これを仮に「ページ」と呼ぶ）を占有すると決めておけば、実行時の`nx`の値から`initialAddress`を引いて`maxSize`で割り算することで、現在何番目のページを読んでいるのかが分かる（それを求めるのがLinker.hsの`toPageId :: Word32 -> PageId`）。その文脈情報を元に、リンカで既に行っておいた名前解決をするのが`resolveLabel :: Word32 -> Program -> Label -> Maybe Word32`。ん？なんで`Maybe`があるんだ？ああなるほど。[脚注4](#myfootnote4)の時点では「未定義のラベルを踏んだら実行時エラー（あれ？こういうのはリンカが検出してくれるんじゃないの？）」って思ったけど、つまりこういうことか：

- 現状のリンカもどきは、『定義されているラベルに対してアドレスを教えること』（まあ要するにDNSみたいな）に関してはチェックする  
- しかし、『未定義のラベルを要求するプログラムを排除する』作業はしていない（！！！）  
- 故に、プログラムが未定義のラベルを要求したら実行時エラーを返さねばならない

なるほどなぁ。確かに『未定義のラベルを要求するプログラムを排除する』記憶が皆無だ。組んでる当時の私はここらへんの論理整合性は考えていなくて、Haskellの型システムに従って「なんか`Maybe`要るっぽいし`Maybe`つけるかー」「あ、この`resolveLabel`に`Maybe`があるし、失敗したらとりあえず実行時エラーにしなきゃ」みたいな気持ちで組んだんだろうけど。

### 話を戻す

さてさてExecute.hsに戻ろう。

**_執筆中_**



## 脚注
<a name="myfootnote1">1</a>:リパライン語kinunsares「公開する」由来  
<a name="myfootnote2">2</a>:リパライン語xokison「別の場所で」由来  
<a name="myfootnote3">3</a>:`initialAddress`は定数値`0x14830000`である。  
<a name="myfootnote4">4</a>:なお、実行時エラーは現状では「実行終了のタイミングで`f5`が元の値に戻っていない」「未定義のラベルを踏んだ（あれ？こういうのはリンカが検出してくれるんじゃないの？）」「nxが命令のアドレスとしては無効なアドレスを指している」の3パターンがある。  
<a name="myfootnote5">5</a>:`initialF5`は定数値`0x6d7aa0f8`である。  
<a name="myfootnote6">6</a>:よく考えてみると、`RPlusNum`とかじゃなくて`RPlusNum_Deref`とか`RPlusNum_Addr`の方が普通にずっと分かりやすいのでは。  
<a name="myfootnote7">7</a>:説明が（現在および未来の私にとって）必要なので（現在の私が読んで未来の私のために）書く。  

## テスト方法

```sh
$ cd 2003lk
$ stack build --ghc-options=-Wall
$ stack exec xarzniar-exe  #デモ
$ stack exec -- xarzniar-exe quicksort #単独で実行
$ stack exec -- xarzniar-exe -x quicksort #ステップ実行
```
