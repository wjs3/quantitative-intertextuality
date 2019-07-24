library(stringi)
library(Unicode)

# HTML Entities
#  - couldn't find a CRAN package for this ... ?

htmlEnt <- new.env(hash=T)
assign("apos", "'", envir=htmlEnt)
assign("iexcl", intToUtf8(as.u_char(161)), envir=htmlEnt)
assign("cent", intToUtf8(as.u_char(162)), envir=htmlEnt)
assign("pound", intToUtf8(as.u_char(163)), envir=htmlEnt)
assign("curren", intToUtf8(as.u_char(164)), envir=htmlEnt)
assign("yen", intToUtf8(as.u_char(165)), envir=htmlEnt)
assign("brvbar", intToUtf8(as.u_char(166)), envir=htmlEnt)
assign("sect", intToUtf8(as.u_char(167)), envir=htmlEnt)
assign("uml", intToUtf8(as.u_char(168)), envir=htmlEnt)
assign("copy", intToUtf8(as.u_char(169)), envir=htmlEnt)
assign("ordf", intToUtf8(as.u_char(170)), envir=htmlEnt)
assign("laquo", intToUtf8(as.u_char(171)), envir=htmlEnt)
assign("not", intToUtf8(as.u_char(172)), envir=htmlEnt)
assign("shy", intToUtf8(as.u_char(173)), envir=htmlEnt)
assign("reg", intToUtf8(as.u_char(174)), envir=htmlEnt)
assign("macr", intToUtf8(as.u_char(175)), envir=htmlEnt)
assign("deg", intToUtf8(as.u_char(176)), envir=htmlEnt)
assign("plusmn", intToUtf8(as.u_char(177)), envir=htmlEnt)
assign("sup2", intToUtf8(as.u_char(178)), envir=htmlEnt)
assign("sup3", intToUtf8(as.u_char(179)), envir=htmlEnt)
assign("acute", intToUtf8(as.u_char(180)), envir=htmlEnt)
assign("micro", intToUtf8(as.u_char(181)), envir=htmlEnt)
assign("para", intToUtf8(as.u_char(182)), envir=htmlEnt)
assign("middot", intToUtf8(as.u_char(183)), envir=htmlEnt)
assign("cedil", intToUtf8(as.u_char(184)), envir=htmlEnt)
assign("sup1", intToUtf8(as.u_char(185)), envir=htmlEnt)
assign("ordm", intToUtf8(as.u_char(186)), envir=htmlEnt)
assign("raquo", intToUtf8(as.u_char(187)), envir=htmlEnt)
assign("frac14", intToUtf8(as.u_char(188)), envir=htmlEnt)
assign("frac12", intToUtf8(as.u_char(189)), envir=htmlEnt)
assign("frac34", intToUtf8(as.u_char(190)), envir=htmlEnt)
assign("iquest", intToUtf8(as.u_char(191)), envir=htmlEnt)
assign("Agrave", intToUtf8(as.u_char(192)), envir=htmlEnt)
assign("Aacute", intToUtf8(as.u_char(193)), envir=htmlEnt)
assign("Acirc", intToUtf8(as.u_char(194)), envir=htmlEnt)
assign("Atilde", intToUtf8(as.u_char(195)), envir=htmlEnt)
assign("Auml", intToUtf8(as.u_char(196)), envir=htmlEnt)
assign("Aring", intToUtf8(as.u_char(197)), envir=htmlEnt)
assign("AElig", intToUtf8(as.u_char(198)), envir=htmlEnt)
assign("Ccedil", intToUtf8(as.u_char(199)), envir=htmlEnt)
assign("Egrave", intToUtf8(as.u_char(200)), envir=htmlEnt)
assign("Eacute", intToUtf8(as.u_char(201)), envir=htmlEnt)
assign("Ecirc", intToUtf8(as.u_char(202)), envir=htmlEnt)
assign("Euml", intToUtf8(as.u_char(203)), envir=htmlEnt)
assign("Igrave", intToUtf8(as.u_char(204)), envir=htmlEnt)
assign("Iacute", intToUtf8(as.u_char(205)), envir=htmlEnt)
assign("Icirc", intToUtf8(as.u_char(206)), envir=htmlEnt)
assign("Iuml", intToUtf8(as.u_char(207)), envir=htmlEnt)
assign("ETH", intToUtf8(as.u_char(208)), envir=htmlEnt)
assign("Ntilde", intToUtf8(as.u_char(209)), envir=htmlEnt)
assign("Ograve", intToUtf8(as.u_char(210)), envir=htmlEnt)
assign("Oacute", intToUtf8(as.u_char(211)), envir=htmlEnt)
assign("Ocirc", intToUtf8(as.u_char(212)), envir=htmlEnt)
assign("Otilde", intToUtf8(as.u_char(213)), envir=htmlEnt)
assign("Ouml", intToUtf8(as.u_char(214)), envir=htmlEnt)
assign("times", intToUtf8(as.u_char(215)), envir=htmlEnt)
assign("Oslash", intToUtf8(as.u_char(216)), envir=htmlEnt)
assign("Ugrave", intToUtf8(as.u_char(217)), envir=htmlEnt)
assign("Uacute", intToUtf8(as.u_char(218)), envir=htmlEnt)
assign("Ucirc", intToUtf8(as.u_char(219)), envir=htmlEnt)
assign("Uuml", intToUtf8(as.u_char(220)), envir=htmlEnt)
assign("Yacute", intToUtf8(as.u_char(221)), envir=htmlEnt)
assign("THORN", intToUtf8(as.u_char(222)), envir=htmlEnt)
assign("szlig", intToUtf8(as.u_char(223)), envir=htmlEnt)
assign("agrave", intToUtf8(as.u_char(224)), envir=htmlEnt)
assign("aacute", intToUtf8(as.u_char(225)), envir=htmlEnt)
assign("acirc", intToUtf8(as.u_char(226)), envir=htmlEnt)
assign("atilde", intToUtf8(as.u_char(227)), envir=htmlEnt)
assign("auml", intToUtf8(as.u_char(228)), envir=htmlEnt)
assign("aring", intToUtf8(as.u_char(229)), envir=htmlEnt)
assign("aelig", intToUtf8(as.u_char(230)), envir=htmlEnt)
assign("ccedil", intToUtf8(as.u_char(231)), envir=htmlEnt)
assign("egrave", intToUtf8(as.u_char(232)), envir=htmlEnt)
assign("eacute", intToUtf8(as.u_char(233)), envir=htmlEnt)
assign("ecirc", intToUtf8(as.u_char(234)), envir=htmlEnt)
assign("euml", intToUtf8(as.u_char(235)), envir=htmlEnt)
assign("igrave", intToUtf8(as.u_char(236)), envir=htmlEnt)
assign("iacute", intToUtf8(as.u_char(237)), envir=htmlEnt)
assign("icirc", intToUtf8(as.u_char(238)), envir=htmlEnt)
assign("iuml", intToUtf8(as.u_char(239)), envir=htmlEnt)
assign("eth", intToUtf8(as.u_char(240)), envir=htmlEnt)
assign("ntilde", intToUtf8(as.u_char(241)), envir=htmlEnt)
assign("ograve", intToUtf8(as.u_char(242)), envir=htmlEnt)
assign("oacute", intToUtf8(as.u_char(243)), envir=htmlEnt)
assign("ocirc", intToUtf8(as.u_char(244)), envir=htmlEnt)
assign("otilde", intToUtf8(as.u_char(245)), envir=htmlEnt)
assign("ouml", intToUtf8(as.u_char(246)), envir=htmlEnt)
assign("divide", intToUtf8(as.u_char(247)), envir=htmlEnt)
assign("oslash", intToUtf8(as.u_char(248)), envir=htmlEnt)
assign("ugrave", intToUtf8(as.u_char(249)), envir=htmlEnt)
assign("uacute", intToUtf8(as.u_char(250)), envir=htmlEnt)
assign("ucirc", intToUtf8(as.u_char(251)), envir=htmlEnt)
assign("uuml", intToUtf8(as.u_char(252)), envir=htmlEnt)
assign("yacute", intToUtf8(as.u_char(253)), envir=htmlEnt)
assign("thorn", intToUtf8(as.u_char(254)), envir=htmlEnt)
assign("yuml", intToUtf8(as.u_char(255)), envir=htmlEnt)
assign("fnof", intToUtf8(as.u_char(402)), envir=htmlEnt)
assign("Alpha", intToUtf8(as.u_char(913)), envir=htmlEnt)
assign("Beta", intToUtf8(as.u_char(914)), envir=htmlEnt)
assign("Gamma", intToUtf8(as.u_char(915)), envir=htmlEnt)
assign("Delta", intToUtf8(as.u_char(916)), envir=htmlEnt)
assign("Epsilon", intToUtf8(as.u_char(917)), envir=htmlEnt)
assign("Zeta", intToUtf8(as.u_char(918)), envir=htmlEnt)
assign("Eta", intToUtf8(as.u_char(919)), envir=htmlEnt)
assign("Theta", intToUtf8(as.u_char(920)), envir=htmlEnt)
assign("Iota", intToUtf8(as.u_char(921)), envir=htmlEnt)
assign("Kappa", intToUtf8(as.u_char(922)), envir=htmlEnt)
assign("Lambda", intToUtf8(as.u_char(923)), envir=htmlEnt)
assign("Mu", intToUtf8(as.u_char(924)), envir=htmlEnt)
assign("Nu", intToUtf8(as.u_char(925)), envir=htmlEnt)
assign("Xi", intToUtf8(as.u_char(926)), envir=htmlEnt)
assign("Omicron", intToUtf8(as.u_char(927)), envir=htmlEnt)
assign("Pi", intToUtf8(as.u_char(928)), envir=htmlEnt)
assign("Rho", intToUtf8(as.u_char(929)), envir=htmlEnt)
assign("Sigma", intToUtf8(as.u_char(931)), envir=htmlEnt)
assign("Tau", intToUtf8(as.u_char(932)), envir=htmlEnt)
assign("Upsilon", intToUtf8(as.u_char(933)), envir=htmlEnt)
assign("Phi", intToUtf8(as.u_char(934)), envir=htmlEnt)
assign("Chi", intToUtf8(as.u_char(935)), envir=htmlEnt)
assign("Psi", intToUtf8(as.u_char(936)), envir=htmlEnt)
assign("Omega", intToUtf8(as.u_char(937)), envir=htmlEnt)
assign("beta", intToUtf8(as.u_char(946)), envir=htmlEnt)
assign("gamma", intToUtf8(as.u_char(947)), envir=htmlEnt)
assign("delta", intToUtf8(as.u_char(948)), envir=htmlEnt)
assign("epsilon", intToUtf8(as.u_char(949)), envir=htmlEnt)
assign("zeta", intToUtf8(as.u_char(950)), envir=htmlEnt)
assign("eta", intToUtf8(as.u_char(951)), envir=htmlEnt)
assign("theta", intToUtf8(as.u_char(952)), envir=htmlEnt)
assign("iota", intToUtf8(as.u_char(953)), envir=htmlEnt)
assign("kappa", intToUtf8(as.u_char(954)), envir=htmlEnt)
assign("lambda", intToUtf8(as.u_char(955)), envir=htmlEnt)
assign("mu", intToUtf8(as.u_char(956)), envir=htmlEnt)
assign("nu", intToUtf8(as.u_char(957)), envir=htmlEnt)
assign("xi", intToUtf8(as.u_char(958)), envir=htmlEnt)
assign("omicron", intToUtf8(as.u_char(959)), envir=htmlEnt)
assign("pi", intToUtf8(as.u_char(960)), envir=htmlEnt)
assign("rho", intToUtf8(as.u_char(961)), envir=htmlEnt)
assign("sigmaf", intToUtf8(as.u_char(962)), envir=htmlEnt)
assign("sigma", intToUtf8(as.u_char(963)), envir=htmlEnt)
assign("tau", intToUtf8(as.u_char(964)), envir=htmlEnt)
assign("upsilon", intToUtf8(as.u_char(965)), envir=htmlEnt)
assign("phi", intToUtf8(as.u_char(966)), envir=htmlEnt)
assign("chi", intToUtf8(as.u_char(967)), envir=htmlEnt)
assign("psi", intToUtf8(as.u_char(968)), envir=htmlEnt)
assign("omega", intToUtf8(as.u_char(969)), envir=htmlEnt)
assign("thetasym", intToUtf8(as.u_char(977)), envir=htmlEnt)
assign("upsih", intToUtf8(as.u_char(978)), envir=htmlEnt)
assign("piv", intToUtf8(as.u_char(982)), envir=htmlEnt)
assign("bull", intToUtf8(as.u_char(8226)), envir=htmlEnt)
assign("hellip", intToUtf8(as.u_char(8230)), envir=htmlEnt)
assign("prime", intToUtf8(as.u_char(8242)), envir=htmlEnt)
assign("Prime", intToUtf8(as.u_char(8243)), envir=htmlEnt)
assign("oline", intToUtf8(as.u_char(8254)), envir=htmlEnt)
assign("frasl", intToUtf8(as.u_char(8260)), envir=htmlEnt)
assign("weierp", intToUtf8(as.u_char(8472)), envir=htmlEnt)
assign("image", intToUtf8(as.u_char(8465)), envir=htmlEnt)
assign("real", intToUtf8(as.u_char(8476)), envir=htmlEnt)
assign("trade", intToUtf8(as.u_char(8482)), envir=htmlEnt)
assign("alefsym", intToUtf8(as.u_char(8501)), envir=htmlEnt)
assign("larr", intToUtf8(as.u_char(8592)), envir=htmlEnt)
assign("uarr", intToUtf8(as.u_char(8593)), envir=htmlEnt)
assign("rarr", intToUtf8(as.u_char(8594)), envir=htmlEnt)
assign("darr", intToUtf8(as.u_char(8595)), envir=htmlEnt)
assign("harr", intToUtf8(as.u_char(8596)), envir=htmlEnt)
assign("crarr", intToUtf8(as.u_char(8629)), envir=htmlEnt)
assign("lArr", intToUtf8(as.u_char(8656)), envir=htmlEnt)
assign("uArr", intToUtf8(as.u_char(8657)), envir=htmlEnt)
assign("rArr", intToUtf8(as.u_char(8658)), envir=htmlEnt)
assign("dArr", intToUtf8(as.u_char(8659)), envir=htmlEnt)
assign("hArr", intToUtf8(as.u_char(8660)), envir=htmlEnt)
assign("forall", intToUtf8(as.u_char(8704)), envir=htmlEnt)
assign("part", intToUtf8(as.u_char(8706)), envir=htmlEnt)
assign("exist", intToUtf8(as.u_char(8707)), envir=htmlEnt)
assign("empty", intToUtf8(as.u_char(8709)), envir=htmlEnt)
assign("nabla", intToUtf8(as.u_char(8711)), envir=htmlEnt)
assign("isin", intToUtf8(as.u_char(8712)), envir=htmlEnt)
assign("notin", intToUtf8(as.u_char(8713)), envir=htmlEnt)
assign("ni", intToUtf8(as.u_char(8715)), envir=htmlEnt)
assign("prod", intToUtf8(as.u_char(8719)), envir=htmlEnt)
assign("sum", intToUtf8(as.u_char(8721)), envir=htmlEnt)
assign("minus", intToUtf8(as.u_char(8722)), envir=htmlEnt)
assign("lowast", intToUtf8(as.u_char(8727)), envir=htmlEnt)
assign("radic", intToUtf8(as.u_char(8730)), envir=htmlEnt)
assign("prop", intToUtf8(as.u_char(8733)), envir=htmlEnt)
assign("infin", intToUtf8(as.u_char(8734)), envir=htmlEnt)
assign("ang", intToUtf8(as.u_char(8736)), envir=htmlEnt)
assign("and", intToUtf8(as.u_char(8743)), envir=htmlEnt)
assign("or", intToUtf8(as.u_char(8744)), envir=htmlEnt)
assign("cap", intToUtf8(as.u_char(8745)), envir=htmlEnt)
assign("cup", intToUtf8(as.u_char(8746)), envir=htmlEnt)
assign("int", intToUtf8(as.u_char(8747)), envir=htmlEnt)
assign("there4", intToUtf8(as.u_char(8756)), envir=htmlEnt)
assign("sim", intToUtf8(as.u_char(8764)), envir=htmlEnt)
assign("cong", intToUtf8(as.u_char(8773)), envir=htmlEnt)
assign("asymp", intToUtf8(as.u_char(8776)), envir=htmlEnt)
assign("ne", intToUtf8(as.u_char(8800)), envir=htmlEnt)
assign("equiv", intToUtf8(as.u_char(8801)), envir=htmlEnt)
assign("le", intToUtf8(as.u_char(8804)), envir=htmlEnt)
assign("ge", intToUtf8(as.u_char(8805)), envir=htmlEnt)
assign("sub", intToUtf8(as.u_char(8834)), envir=htmlEnt)
assign("sup", intToUtf8(as.u_char(8835)), envir=htmlEnt)
assign("nsub", intToUtf8(as.u_char(8836)), envir=htmlEnt)
assign("sube", intToUtf8(as.u_char(8838)), envir=htmlEnt)
assign("supe", intToUtf8(as.u_char(8839)), envir=htmlEnt)
assign("oplus", intToUtf8(as.u_char(8853)), envir=htmlEnt)
assign("otimes", intToUtf8(as.u_char(8855)), envir=htmlEnt)
assign("perp", intToUtf8(as.u_char(8869)), envir=htmlEnt)
assign("sdot", intToUtf8(as.u_char(8901)), envir=htmlEnt)
assign("lceil", intToUtf8(as.u_char(8968)), envir=htmlEnt)
assign("rceil", intToUtf8(as.u_char(8969)), envir=htmlEnt)
assign("lfloor", intToUtf8(as.u_char(8970)), envir=htmlEnt)
assign("rfloor", intToUtf8(as.u_char(8971)), envir=htmlEnt)
assign("lang", intToUtf8(as.u_char(9001)), envir=htmlEnt)
assign("rang", intToUtf8(as.u_char(9002)), envir=htmlEnt)
assign("loz", intToUtf8(as.u_char(9674)), envir=htmlEnt)
assign("spades", intToUtf8(as.u_char(9824)), envir=htmlEnt)
assign("clubs", intToUtf8(as.u_char(9827)), envir=htmlEnt)
assign("hearts", intToUtf8(as.u_char(9829)), envir=htmlEnt)
assign("diams", intToUtf8(as.u_char(9830)), envir=htmlEnt)
assign("quot", intToUtf8(as.u_char(34)), envir=htmlEnt)
assign("amp", intToUtf8(as.u_char(38)), envir=htmlEnt)
assign("lt", intToUtf8(as.u_char(60)), envir=htmlEnt)
assign("gt", intToUtf8(as.u_char(62)), envir=htmlEnt)
assign("OElig", intToUtf8(as.u_char(338)), envir=htmlEnt)
assign("oelig", intToUtf8(as.u_char(339)), envir=htmlEnt)
assign("Scaron", intToUtf8(as.u_char(352)), envir=htmlEnt)
assign("scaron", intToUtf8(as.u_char(353)), envir=htmlEnt)
assign("Yuml", intToUtf8(as.u_char(376)), envir=htmlEnt)
assign("circ", intToUtf8(as.u_char(710)), envir=htmlEnt)
assign("tilde", intToUtf8(as.u_char(732)), envir=htmlEnt)
assign("ensp", intToUtf8(as.u_char(8194)), envir=htmlEnt)
assign("emsp", intToUtf8(as.u_char(8195)), envir=htmlEnt)
assign("thinsp", intToUtf8(as.u_char(8201)), envir=htmlEnt)
assign("zwnj", intToUtf8(as.u_char(8204)), envir=htmlEnt)
assign("zwj", intToUtf8(as.u_char(8205)), envir=htmlEnt)
assign("lrm", intToUtf8(as.u_char(8206)), envir=htmlEnt)
assign("rlm", intToUtf8(as.u_char(8207)), envir=htmlEnt)
assign("ndash", intToUtf8(as.u_char(8211)), envir=htmlEnt)
assign("mdash", intToUtf8(as.u_char(8212)), envir=htmlEnt)
assign("lsquo", intToUtf8(as.u_char(8216)), envir=htmlEnt)
assign("rsquo", intToUtf8(as.u_char(8217)), envir=htmlEnt)
assign("sbquo", intToUtf8(as.u_char(8218)), envir=htmlEnt)
assign("ldquo", intToUtf8(as.u_char(8220)), envir=htmlEnt)
assign("rdquo", intToUtf8(as.u_char(8221)), envir=htmlEnt)
assign("bdquo", intToUtf8(as.u_char(8222)), envir=htmlEnt)
assign("dagger", intToUtf8(as.u_char(8224)), envir=htmlEnt)
assign("Dagger", intToUtf8(as.u_char(8225)), envir=htmlEnt)
assign("permil", intToUtf8(as.u_char(8240)), envir=htmlEnt)
assign("lsaquo", intToUtf8(as.u_char(8249)), envir=htmlEnt)
assign("rsaquo", intToUtf8(as.u_char(8250)), envir=htmlEnt)
assign("euro", intToUtf8(as.u_char(8364)), envir=htmlEnt)


htmlUnescapeEnt <- Vectorize(USE.NAMES=F, FUN=function(name) {
  code <- stri_trim_both(name)
  code <- stri_replace_first_regex(code, "^&", "")
  code <- stri_replace_first_regex(code, ";$", "")

  mget(code, envir=htmlEnt, ifnotfound = name)[[1]]
})

htmlUnescapeText <- Vectorize(USE.NAMES=F, FUN=function(text) {
  m <- stri_locate_all_regex(text, "&[a-zA-Z]{2,8};")[[1]]

  if (is.na(m[1])) {
    return(text)
  }

  # reverse order of matches, so later replacements don't change
  # character indices of earlier matches

  m <- matrix(m[nrow(m):1,], ncol=2)

  apply(m, 1, function(row) {
    text_ <- text
    ent <- stri_sub(text_, from=row[1], to=row[2])
    replacement <- htmlUnescapeEnt(ent)
    stri_sub(text_, from=row[1], to=row[2]) <- replacement
    text <<- text_
  })

  return(text)
})

#

regex_or <- function(...) {
  or_expression <- paste(sep="|", ...)
  paste(sep="", "(:?", or_expression, ")")
}

# contractions = "(?i)(\w+)(n['’′]t|['’′]ve|['’′]ll|['’′]d|['’′]re|['’′]s|['’′]m)$"
contractions <- "(\\w+)(n['’′]t|['’′]ve|['’′]ll|['’′]d|['’′]re|['’′]s|['’′]m)$"
whitespace <- "[\\s\\p{Zs}]+"
# punctSeq   = punctChars+"+";  //'anthem'. => ' anthem '.
punctChars <- "['\"“”‘’.?!…,:;]"
punctSeq <- "['\"“”‘’]+|[.?!,…]+|[:;]+"  #'anthem'. => ' anthem ' .
entity <- "&(?:amp|lt|gt|quot);"

#  URLs

# BTO 2012-06: everyone thinks the daringfireball regex should be better, but they're wrong.
# If you actually empirically test it the results are bad.
# Please see https://github.com/brendano/ark-tweet-nlp/pull/9

urlStart1 <- "(?:https?://|\\bwww\\.)"
commonTLDs <- "(?:com|org|edu|gov|net|mil|aero|asia|biz|cat|coop|info|int|jobs|mobi|museum|name|pro|tel|travel|xxx)"
ccTLDs <- paste(sep="",
  "(?:ac|ad|ae|af|ag|ai|al|am|an|ao|aq|ar|as|at|au|aw|ax|az|ba|bb|bd|be|bf|bg|bh|bi|bj|bm|bn|bo|br|bs|bt|",
  "bv|bw|by|bz|ca|cc|cd|cf|cg|ch|ci|ck|cl|cm|cn|co|cr|cs|cu|cv|cx|cy|cz|dd|de|dj|dk|dm|do|dz|ec|ee|eg|eh|",
  "er|es|et|eu|fi|fj|fk|fm|fo|fr|ga|gb|gd|ge|gf|gg|gh|gi|gl|gm|gn|gp|gq|gr|gs|gt|gu|gw|gy|hk|hm|hn|hr|ht|",
  "hu|id|ie|il|im|in|io|iq|ir|is|it|je|jm|jo|jp|ke|kg|kh|ki|km|kn|kp|kr|kw|ky|kz|la|lb|lc|li|lk|lr|ls|lt|",
  "lu|lv|ly|ma|mc|md|me|mg|mh|mk|ml|mm|mn|mo|mp|mq|mr|ms|mt|mu|mv|mw|mx|my|mz|na|nc|ne|nf|ng|ni|nl|no|np|",
  "nr|nu|nz|om|pa|pe|pf|pg|ph|pk|pl|pm|pn|pr|ps|pt|pw|py|qa|re|ro|rs|ru|rw|sa|sb|sc|sd|se|sg|sh|si|sj|sk|",
  "sl|sm|sn|so|sr|ss|st|su|sv|sy|sz|tc|td|tf|tg|th|tj|tk|tl|tm|tn|to|tp|tr|tt|tv|tw|tz|ua|ug|uk|us|uy|uz|",
  "va|vc|ve|vg|vi|vn|vu|wf|ws|ye|yt|za|zm|zw)") # TODO: remove obscure country domains?
urlStart2 <- paste(sep="",
  "\\b(?:[A-Za-z\\d-])+(?:\\.[A-Za-z0-9]+){0,3}\\.",
  "(?:", commonTLDs, "|", ccTLDs, ")",
  "(?:\\.", ccTLDs, ")?(?=\\W|$)")
urlBody <- "(?:[^\\.\\s<>][^\\s<>]*?)?"
urlExtraCrapBeforeEnd <- paste(sep="", "(?:", punctChars, "|", entity, ")+?")
urlEnd <- "(?:\\.\\.+|[<>]|\\s|$)"
url <- paste(sep="",
  "(?:", urlStart1, "|", urlStart2, ")",
  urlBody,
  "(?=", "(?:", urlExtraCrapBeforeEnd, ")?", urlEnd, ")")

# Numeric
timeLike <- "\\d+(?::\\d+){1,2}"
# numNum     = "\\d+\\.\\d+";
numberWithCommas <- paste(sep="", "(?:(?<!\\d)\\d{1,3},)+?\\d{3}", "(?=(?:[^,\\d]|$))")
numComb <- "\\p{Sc}?\\d+(?:\\.\\d+)+%?"

# Abbreviations
boundaryNotDot <- paste(sep="", "(?:$|\\s|[“\\u0022?!,:;]|", entity, ")")
aa1 <- paste(sep="", "(?:[A-Za-z]\\.){2,}(?=", boundaryNotDot, ")")
aa2 <- paste(sep="", "[^A-Za-z](?:[A-Za-z]\\.){1,}[A-Za-z](?=", boundaryNotDot, ")")
standardAbbreviations <- "\\b(?:[Mm]r|[Mm]rs|[Mm]s|[Dd]r|[Ss]r|[Jj]r|[Rr]ep|[Ss]en|[Ss]t)\\."
arbitraryAbbrev <- paste(sep="", "(?:", aa1, "|", aa2, "|", standardAbbreviations, ")")
separators <- "(?:--+|―|—|~|–|=)"
decorations <- "(?:[♫♪]+|[★☆]+|[♥❤♡]+|[\\u2639-\\u263b]+|[\\ue001-\\uebbb]+)"
thingsThatSplitWords <- "[^\\s\\.,?\"]"
embeddedApostrophe <- paste(sep="",
  thingsThatSplitWords, "+['’′]", thingsThatSplitWords, "*")

# Emoticons
#normalEyes = "(?iu)[:=]" # 8 and x are eyes but cause problems
normalEyes <- "[:=]" # 8 and x are eyes but cause problems
wink <- "[;]"
noseArea <- "(?:|-|[^a-zA-Z0-9 ])" # doesn't get :'-(
happyMouths <- "[D\\)\\]\\}]+"
sadMouths <- "[\\(\\[\\{]+"
tongue <- "[pPd3]+"
otherMouths <- "(?:[oO]+|[/\\\\]+|[vV]+|[Ss]+|[|]+)" # remove forward slash if http://'s aren't cleaned

# mouth repetition examples:
# @aliciakeys Put it in a love song :-))
# @hellocalyclops =))=))=)) Oh well

bfLeft <- "(♥|0|o|°|v|\\$|t|x|;|\\u0CA0|@|ʘ|•|・|◕|\\^|¬|\\*)"
bfCenter <- "(?:[\\.]|[_-]+)"
bfRight <- "\\2"
s3 <- "(?:--['\"])"
s4 <- "(?:<|&lt;|>|&gt;)[\\._-]+(?:<|&lt;|>|&gt;)"
s5 <- "(?:[.][_]+[.])"
basicface <- paste(sep="",
  "(?:(?i)", bfLeft, bfCenter, bfRight, ")|", s3,  "|", s4, "|", s5)

eeLeft <- "[＼\\\\ƪԄ\\(（<>;ヽ\\-=~\\*]+"
eeRight <- "[\\-=\\);'\\u0022<>ʃ）/／ノﾉ丿╯σっµ~\\*]+"
eeSymbol <- "[^A-Za-z0-9\\s\\(\\)\\*:=-]"
eastEmote <- paste(sep="", eeLeft, "(?:", basicface, "|", eeSymbol, ")+", eeRight)

# from twokenize.py
oOEmote <- paste(sep="", "(?:[oO]", bfCenter, "[oO])")

emoticon <- regex_or(
  # Standard version  :) :( :] :D :P
  paste(sep="",
    "(?:>|&gt;)?",  regex_or(normalEyes, wink), regex_or(noseArea, "[Oo]"),
    regex_or(
      paste(sep="", tongue, "(?=\\W|$|RT|rt|Rt)"),
      paste(sep="", otherMouths, "(?=\\W|$|RT|rt|Rt)"), sadMouths, happyMouths)),

  # reversed version (: D:  use positive lookbehind to remove "(word):"
  # because eyes on the right side is more ambiguous with the standard usage of : ;
  paste(sep="",
    "(?<=(?: |^))",
    regex_or(sadMouths, happyMouths, otherMouths),
    noseArea,
    regex_or(normalEyes, wink),
    "(?:<|&lt;)?"),

  # inspired by http://en.wikipedia.org/wiki/User:Scapler/emoticons#East_Asian_style
  # eastEmote.replaceFirst("2", "1") # CWF: what does this replacement do?
  eastEmote, basicface
  # iOS 'emoji' characters (some smileys, some symbols) [\ue001-\uebbb]
  # TODO should try a big precompiled lexicon from Wikipedia, Dan Ramage told me (BTO) he does this
  )

Hearts <- "(?:<+/?3+)+"  # the other hearts are in decorations
Arrows <- paste(sep="",
  "(?:<*[-―—=]*>+|<+[-―—=]*>*)", "|",
  "[", intToUtf8(as.u_char(u_blocks()$Arrows)), "]+")

# BTO 2011-06: restored Hashtag, AtMention protection (dropped in original scala port) because it fixes
# "hello (#hashtag)" ==> "hello (#hashtag )"  WRONG
# "hello (#hashtag)" ==> "hello ( #hashtag )"  RIGHT
# "hello (@person)" ==> "hello (@person )"  WRONG
# "hello (@person)" ==> "hello ( @person )"  RIGHT
# ... Some sort of weird interaction with edgepunct I guess, because edgepunct
# has poor content-symbol detection.

# This also gets #1 #40 which probably aren't hashtags .. but good as tokens.
# If you want good hashtag identification, use a different regex.
Hashtag <- "#[a-zA-Z0-9_]+" # optional: lookbehind for \b
# optional: lookbehind for \b, max length 15
AtMention <- "[@＠][a-zA-Z0-9_]+"

# I was worried this would conflict with at-mentions
# but seems ok in sample of 5800: 7 changes all email fixes
# http://www.regular-expressions.info/email.html
Bound <- "(?:\\W|^|$)"
Email <- paste(sep="",
  "(?<=", Bound, ")",
  "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,4}",
  "(?=", Bound, ")")

# We will be tokenizing using these regexps as delimiters
# Additionally, these things are "protected", meaning they shouldn't be further split themselves.
Protected <- regex_or(
  Hearts,
  url,
  Email,
  timeLike,
  # numNum,
  numberWithCommas,
  numComb,
  emoticon,
  Arrows,
  entity,
  punctSeq,
  arbitraryAbbrev,
  separators,
  decorations,
  embeddedApostrophe,
  Hashtag,
  AtMention)

# Edge punctuation
# Want: 'foo' => ' foo '
# While also:   don't => don't
# the first is considered "edge punctuation".
# the second is word-internal punctuation -- don't want to mess with it.
# BTO (2011-06): the edgepunct system seems to be the #1 source of problems these days.
# I remember it causing lots of trouble in the past as well.  Would be good to revisit or eliminate.

# Note the 'smart quotes' (http://en.wikipedia.org/wiki/Smart_quotes)
edgePunctChars <- "'\"“”‘’«»{}\\(\\)\\[\\]\\*&" # add \\p{So}? (symbols)
edgePunct <- paste(sep="", "[", edgePunctChars, "]")
notEdgePunct <- "[a-zA-Z0-9]" # content characters
offEdge <- "(^|$|:|;|\\s|\\.|,)"  # colon here gets "(hello):" ==> "( hello ):"
EdgePunctLeft <- paste(sep="", offEdge, "(", edgePunct, "+)(", notEdgePunct, ")")
EdgePunctRight <- paste(sep="", "(", notEdgePunct, ")(", edgePunct, "+)", offEdge)

splitEdgePunct <- function(input) {
  input <- stri_replace_all_regex(input, EdgePunctLeft, "$1$2 $3")
  input <- stri_replace_all_regex(input, EdgePunctRight, "$1 $2$3")
  return(input)
}

# The main work of tokenizing a tweet.
simpleTokenize <- function(text) {

  # Do the no-brainers first
  splitPunctText <- splitEdgePunct(text)

  # CWF: not needed
  # textLength <- nchar(splitPunctText);

  # BTO: the logic here got quite convoluted via the Scala porting detour
  # It would be good to switch back to a nice simple procedural style like in the Python version
  # ... Scala is such a pain.  Never again.

  # Find the matches for subsequences that should be protected,
  # e.g. URLs, 1.0, U.N.K.L.E., 12:53

  badSpans <- stri_locate_all_regex(splitPunctText, Protected)[[1]]

  if (is.na(badSpans[1])) {
    zippedStr <- unlist(stri_split(splitPunctText, regex=whitespace))
  } else {
    bads <- stri_sub(splitPunctText, badSpans)
    goodSpans <- matrix(c(1, badSpans[,2]+1, badSpans[,1]-1, nchar(splitPunctText)), ncol=2)
    goods <- stri_sub(splitPunctText, goodSpans)

    # split tokens in the unprotected "goods"
    #  goods is now a list of lists, with first level indicating
    #  original position in the goods/bads alternation
    goods <- stri_split(goods, regex=whitespace)

    # interleave the goods (first level) and bads
    zippedStr <- c(goods, bads)[order(c(seq_along(goods), seq_along(bads)))]
    # flatten lists into a single vector
    zippedStr <- unlist(zippedStr)
    # get rid of empties
    zippedStr <- zippedStr[zippedStr != ""]
  }

  # BTO: our POS tagger wants "ur" and "you're" to both be one token.
  # Uncomment to get "you 're"
  # zippedStr <- unlist(vSplitToken(zippedStr))

  return(zippedStr)
}

# CWF: left out addAllnonempty:
#   - it was easier to add everything and remove empties later
#
# private static List<String> addAllnonempty(List<String> master, List<String> smaller){
#   for (String s : smaller){
#     String strim = s.trim();
#     if (strim.length() > 0)
#       master.add(strim);
#   }
#   return master;
# }


# "foo   bar " => "foo bar"

squeezeWhitespace <- function(input) {
  output <- stri_replace_all_regex(input, pattern=whitespace, replacement=" ")
  output <- stri_trim_both(output)
  return(output)
}


# Final pass tokenization based on special patterns
splitToken <- function(token) {
  m <- stri_match_first_regex(token, pattern = contractions)

  if (is.na(m[1])) {
    return(token)
  } else {
    return(m[1,2:3])
  }
}

# CWF: vectorized version of above, for convenience
vSplitToken <- function(token.vector) {
  setNames(unlist(lapply(token.vector, splitToken)), NULL)
}

# Assume 'text' has no HTML escaping.
tokenize <- function(text){
  return(simpleTokenize(squeezeWhitespace(text)))
}

#
# Twitter text comes HTML-escaped, so unescape it.
# We also first unescape &amp;'s, in case the text has been buggily double-escaped.
#
normalizeTextForTagger <- function(text) {
  text <- stri_replace_all_fixed(text, pattern = "&amp;", replacement = "&")
  text <- htmlUnescapeText(text)
}

#
# This is intended for raw tweet text -- we do some HTML entity unescaping before running the tagger.
#
# This function normalizes the input text BEFORE calling the tokenizer.
# So the tokens you get back may not exactly correspond to
# substrings of the original text.
#

tokenizeRawTweetText <- function(text) {
  tokens <- tokenize(normalizeTextForTagger(text))
  return(tokens)
}

# vectorized form

twit.tokenize <- Vectorize(tokenizeRawTweetText)

