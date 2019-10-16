(in-package :etap)

(defconstant +tex-base-1-encoding+
  '(;; 0x00
    nil |dotaccent| |fi| |fl|
    |fraction| |hungarumlaut| |Lslash| |lslash|
    |ogonek| |ring| nil |breve|
    |minus| nil |Zcaron| |zcaron|
    ;; 0x10
    |caron| |dotlessi| |dotlessj| |ff|
    |ffi| |ffl| |notequal| |infinity|
    |lessequal| |greaterequal| |partialdiff| |summation|
    |product| |pi| |grave| |quotesingle|
    ;; 0x20
    |space| |exclam| |quotedbl| |numbersign|
    |dollar| |percent| |ampersand| |quoteright|
    |parenleft| |parenright| |asterisk| |plus|
    |comma| |hyphen| |period| |slash|
    ;; 0x30
    |zero| |one| |two| |three|
    |four| |five| |six| |seven|
    |eight| |nine| |colon| |semicolon|
    |less| |equal| |greater| |question|
    ;; 0x40
    |at| |A| |B| |C|
    |D| |E| |F| |G|
    |H| |I| |J| |K|
    |L| |M| |N| |O|
    ;; 0x50
    |P| |Q| |R| |S|
    |T| |U| |V| |W|
    |X| |Y| |Z| |bracketleft|
    |backslash| |bracketright| |asciicircum| |underscore|
    ;; 0x60
    |quoteleft| |a| |b| |c|
    |d| |e| |f| |g|
    |h| |i| |j| |k|
    |l| |m| |n| |o|
    ;; 0x70
    |p| |q| |r| |s|
    |t| |u| |v| |w|
    |x| |y| |z| |braceleft|
    |bar| |braceright| |asciitilde| nil
    ;; 0x80
    |Euro| |integral| |quotesinglbase| |florin|
    |quotedblbase| |ellipsis| |dagger| |daggerdbl|
    |circumflex| |perthousand| |Scaron| |guilsinglleft|
    |OE| |Omega| |radical| |approxequal|
    ;; 0x90
    nil nil nil |quotedblleft|
    |quotedblright| |bullet| |endash| |emdash|
    |tilde| |trademark| |scaron| |guilsinglright|
    |oe| |Delta| |lozenge| |Ydieresis|
    ;; 0xA0
    nil |exclamdown| |cent| |sterling|
    |currency| |yen| |brokenbar| |section|
    |dieresis| |copyright| |ordfeminine| |guillemotleft|
    |logicalnot| |hyphen| |registered| |macron|
    ;; 0xB0
    |degree| |plusminus| |twosuperior| |threesuperior|
    |acute| |mu| |paragraph| |periodcentered|
    |cedilla| |onesuperior| |ordmasculine| |guillemotright|
    |onequarter| |onehalf| |threequarters| |questiondown|
    ;; 0xC0
    |Agrave| |Aacute| |Acircumflex| |Atilde|
    |Adieresis| |Aring| |AE| |Ccedilla|
    |Egrave| |Eacute| |Ecircumflex| |Edieresis|
    |Igrave| |Iacute| |Icircumflex| |Idieresis|
    ;; 0xD0
    |Eth| |Ntilde| |Ograve| |Oacute|
    |Ocircumflex| |Otilde| |Odieresis| |multiply|
    |Oslash| |Ugrave| |Uacute| |Ucircumflex|
    |Udieresis| |Yacute| |Thorn| |germandbls|
    ;; 0xE0
    |agrave| |aacute| |acircumflex| |atilde|
    |adieresis| |aring| |ae| |ccedilla|
    |egrave| |eacute| |ecircumflex| |edieresis|
    |igrave| |iacute| |icircumflex| |idieresis|
    ;; 0xF0
    |eth| |ntilde| |ograve| |oacute|
    |ocircumflex| |otilde| |odieresis| |divide|
    |oslash| |ugrave| |uacute| |ucircumflex|
    |udieresis| |yacute| |thorn| |ydieresis|
    ))
