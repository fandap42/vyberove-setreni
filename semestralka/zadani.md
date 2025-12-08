Komplexní specifikace a teoretický rámec projektu: Využití Taylorova rozvoje pro odhad rozptylu výběrového kvantilu
Úvod do problematiky a kontext zadání
Předkládaný dokument slouží jako vyčerpávající zadání a teoreticko-metodický podklad pro vypracování semestrální práce v rámci kurzu 4ST414 (Výpočetní statistika a výběrová šetření). Na základě požadavku uživatele dochází k posunu od standardních témat zaměřených na odhady průměrů a úhrnů (jak je patrné z dokumentů Schaffartziková 1, Škrobánek 1 a Drážný 1) k pokročilejší problematice odhadu rozptylu nelineárních statistik, konkrétně výběrových kvantilů, s využitím metody linearizace (Taylorova rozvoje).
Tento dokument je koncipován jako expertní manuál, který poskytuje detailní instrukce pro AI agenta (či lidského řešitele) k vytvoření semestrální práce na úrovni "distinction". Cílem není pouze definovat kroky, ale poskytnout hluboký vhled do statistické teorie, která podmiňuje volbu Woodruffovy metody, a nastínit strukturu, která odpovídá nejvyšším akademickým standardům reprezentovaným vzorovou prací Novákové.1 Rozsah a detailnost tohoto reportu reflektují nutnost komplexního pochopení asymptotických vlastností kvantilů, které jsou ve srovnání s lineárními statistikami (průměry) zatíženy specifickými problémy, zejména závislostí rozptylu na hustotě pravděpodobnosti v daném bodě.
V následujících kapitolách bude rozebrána struktura vzorových prací, odvozena matematická teorie linearizace kvantilů, navržen robustní simulační experiment s log-normálním rozdělením a specifikován kód v jazyce R. Důraz je kladen na syntézu poznatků z přiložených materiálů 1 a jejich aplikaci na nový, náročnější problém.
1. Analýza strukturálních a obsahových standardů vzorových prací
Pro úspěšné vygenerování semestrální práce je nezbytné nejprve dekódovat "genom" úspěšných prací v rámci předmětu 4ST413/414. Analýza přiložených dokumentů odhaluje hierarchii kvality a očekávanou strukturu, kterou musí nová práce nejen napodobit, ale v případě technické náročnosti tématu i překonat.

1.1 Standard "Nováková"
1: Etalon akademické rigorozity

Práce Lucie Novákové 1 na téma "Poměrový odhad" představuje nejvyšší standard, ke kterému má nově generovaná práce směřovat. Na rozdíl od jednodušších prací zaměřených na prosté průměry vykazuje práce Novákové několik klíčových znaků, které musí být integrovány do zadání pro AI agenta:
Formální náležitosti a metadata: Dokument nezačíná pouhým nadpisem, ale obsahuje plnohodnotnou hlavičku s abstraktem, klíčovými slovy (Keywords) a JEL klasifikací (např. C10). Tento detail signalizuje orientaci na ekonomickou aplikaci statistiky a musí být v nové práci zachován.
Matematická formalizace: Nováková 1 se nespokojuje s uvedením vzorců, ale definuje populační parametry (např. $R = \mu_y / \mu_x$), výběrové odhady ($r$) a explicitně uvádí vztah pro střední čtvercovou chybu ($MSE \approx D(\hat{\mu}_r)$). Pro projekt zaměřený na kvantily to znamená nutnost formálně definovat populační kvantil jako inverzi distribuční funkce $F^{-1}(p)$ a odvodit jeho asymptotický rozptyl.
Odvození rozsahu výběru: Kritickým prvkem práce 1 je inverze vzorce pro interval spolehlivosti za účelem stanovení minimálního rozsahu výběru $n$ pro požadovanou přesnost $\Delta$. Nováková odvozuje vzorec:

$$n \ge \frac{1}{\Delta^2 / (u_{1-\alpha/2}^2 \sigma_r^2) + 1/N}$$

Nový projekt musí provést analogické odvození pro kvantily, kde $\sigma_r^2$ bude nahrazeno asymptotickým rozptylem kvantilu.
Logická struktura analýzy: Nováková nejprve ověřuje předpoklady (korelace $> 0.5 \cdot v_x/v_y$) na reálných datech (švédské municipality) a teprve poté aplikuje metodu. V našem případě bude reálná data nahrazovat simulace, ale logický krok "ověření předpokladů" (např. analýza šikmosti log-normálního rozdělení) musí zůstat zachován.

1.2 Přínos prací Schaffartziková
1, Škrobánek 1 a Drážný 1

Zatímco Nováková poskytuje formální šablonu, ostatní práce dodávají kontext pro simulační část a práci s rozsahem výběru.
Simulační narativ (Škrobánek 1): Škrobánek detailně popisuje proces generování dat z log-logistického rozdělení, včetně uvedení parametrů $\alpha, \beta, \gamma$. Tento přístup je přímo aplikovatelný na náš požadavek log-normálního rozdělení. Musíme specifikovat parametry $\mu$ a $\sigma$ tak, aby simulovaly reálná ekonomická data (příjmy), která jsou typicky pravostranně zešikmená. Škrobánkova práce rovněž zavádí standard prezentace výsledků formou tabulek pokrytí (coverage probability), což je metrika, kterou musíme u Woodruffovy metody sledovat.
Analýza citlivosti (Schaffartziková 1 a Drážný 1): Obě práce využívají rozsáhlé tabulky (Tabulka 1 v 1) křížící velikost populace $N$ s požadovanou přesností $\Delta$ a spolehlivostí $1-\alpha$. Schaffartziková 1 explicitně řeší otázku, zda "stačí výběr 1000 občanů". Tento prvek "výzkumné otázky" je vhodné přenést i do nové práce: "Stačí standardní rozsahy výběru pro odhad 95% kvantilu u silně sešikmeného rozdělení?"
Dvojí kriteriální úloha (Drážný 1): Drážný zdůrazňuje kompromis mezi přesností a náklady (rozsahem výběru). V kontextu kvantilů je tento kompromis ještě drastičtější, protože rozptyl odhadu v chvostech rozdělení (např. 99. percentil) roste mnohem rychleji než rozptyl průměru.
2. Teoretická východiska: Od průměru ke kvantilu
Jádrem zadání je posun od lineárních statistik (průměr, úhrn) k nelineárním (kvantil). Pro AI agenta je nutné explicitně formulovat teoretický rámec, který vysvětluje, proč pro kvantily nemůžeme použít jednoduchý vzorec typu $S^2/n$ a proč musíme sáhnout po Taylorově linearizaci.
2.1 Definice a vlastnosti výběrového kvantilu
Zatímco výběrový průměr $\bar{y}$ je lineární kombinací pozorování, výběrový kvantil $\hat{Q}_p$ je určen uspořádáním. Nechť $Y$ je náhodná veličina s distribuční funkcí $F(y)$ a hustotou pravděpodobnosti $f(y)$. Populační kvantil řádu $p$ (kde $p \in (0, 1)$) je definován jako inverzní funkce:


$$Q_p = F^{-1}(p) = \inf \{ y : F(y) \ge p \}$$

Výběrový kvantil $\hat{Q}_p$ z prostého náhodného výběru o rozsahu $n$ je pak definován pomocí empirické distribuční funkce $\hat{F}_n(y)$ jako $\hat{Q}_p = \hat{F}_n^{-1}(p)$. V praxi odpovídá $k$-té pořádkové statistice $Y_{(k)}$, kde $k \approx np$.
Zásadním teoretickým poznatkem, který musí být v práci uveden (s odkazem na literaturu typu Särndal et al. nebo Thompson 1), je, že pro konečné výběry nemá výběrový kvantil jednoduchý rozptyl. Musíme se spoléhat na asymptotickou teorii.
2.2 Asymptotické rozdělení a Bahadurův vztah
Pro velká $n$ konverguje výběrový kvantil k normálnímu rozdělení. Tento výsledek, známý jako Bahadurův teorém, uvádí:


$$\hat{Q}_p \approx N \left( Q_p, \frac{p(1-p)}{n [f(Q_p)]^2} \right)$$

Zde narážíme na kritický problém, který odlišuje tuto práci od prací zaměřených na průměry.1 Rozptyl kvantilu závisí na členu $1/[f(Q_p)]^2$. To znamená, že přesnost odhadu kvantilu je nepřímo úměrná čtverci hustoty pravděpodobnosti v daném bodě.
Interpretace: Pokud je v okolí kvantilu "málo dat" (nízká hustota $f(Q_p)$, např. v chvostech log-normálního rozdělení), rozptyl odhadu bude obrovský.
Důsledek pro zadání: Toto je teoretické zdůvodnění, proč potřebujeme Woodruffovu metodu. Přímý odhad $f(Q_p)$ pomocí histogramu nebo jádrových odhadů je numericky nestabilní a závisí na volbě vyhlazovacích parametrů.
2.3 Princip linearizace pomocí Taylorova rozvoje
Metoda, kterou má student (a tedy AI agent) aplikovat, je založena na linearizaci nelineární statistiky. V kontextu prací 1 (poměrový odhad) byla linearizace použita pro funkci dvou proměnných $f(\bar{x}, \bar{y}) = \bar{y}/\bar{x}$. Pro kvantil jde o linearizaci implicitní funkce.
Vycházíme z rovnosti $F(Q_p) = p$. Aplikací Taylorova rozvoje prvního řádu pro empirickou distribuční funkci $\hat{F}$ v bodě $\hat{Q}_p$ kolem bodu $Q_p$ dostáváme:
$$\hat{F}(\hat{Q}_p) \approx \hat{F}(Q_p) + (\hat{Q}_p - Q_p) f(Q_p)$$Jelikož $\hat{F}(\hat{Q}_p) \approx p$ (z definice výběrového kvantilu), můžeme psát:

$$p \approx \hat{F}(Q_p) + (\hat{Q}_p - Q_p) f(Q_p)$$Odtud vyjádříme chybu odhadu:$$(\hat{Q}_p - Q_p) \approx \frac{p - \hat{F}(Q_p)}{f(Q_p)}$$

Tento vztah převádí problém odhadu kvantilu (horizontální vzdálenost) na problém odhadu hodnoty distribuční funkce (vertikální vzdálenost), škálovaný převrácenou hodnotou hustoty.
2.4 Woodruffova metoda intervalové inverze (1952)
Woodruff (1952) navrhl geniální způsob, jak využít výše uvedenou linearizaci bez nutnosti explicitně odhadovat $f(Q_p)$. Místo abychom počítali $f(Q_p)$, konstruujeme interval spolehlivosti pro podíl $p$ a ten promítneme zpět na osu kvantilů.
Postup, který musí být v práci popsán a následně naprogramován, je následující:
Sestrojíme $100(1-\alpha)\%$ interval spolehlivosti pro očekávanou hodnotu distribuční funkce, což je $p$. Protože $\hat{F}(Q_p)$ se chová jako výběrový podíl, směrodatná odchylka je $\sqrt{p(1-p)/n}$.

$$p_L = p - u_{1-\alpha/2} \sqrt{\frac{p(1-p)}{n}}$$
$$p_U = p + u_{1-\alpha/2} \sqrt{\frac{p(1-p)}{n}}$$
Těmto mezním pravděpodobnostem $p_L$ a $p_U$ přiřadíme odpovídající výběrové kvantily (pořádkové statistiky) $y_{(n p_L)}$ a $y_{(n p_U)}$.
Získaný interval $[ \hat{Q}_{p_L}, \hat{Q}_{p_U} ]$ je Woodruffův interval spolehlivosti pro kvantil $Q_p$.
Pokud potřebujeme bodový odhad rozptylu (standard error), využijeme vztahu mezi délkou intervalu a směrodatnou odchylkou u normálního rozdělení:

$$\hat{SE}(\hat{Q}_p) = \frac{\hat{Q}_{p_U} - \hat{Q}_{p_L}}{2 u_{1-\alpha/2}}$$
Tento přístup je výpočetně efektivní a vyhýbá se problémům s odhadem hustoty. V práci musí být zdůrazněno, že tato metoda je standardem v softwarových balících pro výběrová šetření (např. procedura SURVEYFREQ v SAS nebo balíček survey v R).
3. Metodika stanovení rozsahu výběru
Jedním z klíčových požadavků vzorových prací 1 je výpočet minimálního rozsahu výběru $n$ pro požadovanou přesnost $\Delta$. Nováková 1 tento výpočet provádí pro poměrový odhad. AI agent musí provést analogické odvození pro kvantil.
3.1 Odvození vzorce pro kvantily
Požadujeme, aby chyba odhadu nepřekročila $\Delta$ s pravděpodobností $1-\alpha$:


$$P(|\hat{Q}_p - Q_p| \le \Delta) \ge 1-\alpha$$
Využitím asymptotické normality a Woodruffovy aproximace můžeme psát podmínku pro poloviční šířku intervalu:

$$u_{1-\alpha/2} \cdot SE(\hat{Q}_p) \le \Delta$$
Dosazením asymptotického rozptylu:

$$u_{1-\alpha/2} \cdot \frac{\sqrt{p(1-p)}}{\sqrt{n} f(Q_p)} \le \Delta$$
Úpravou pro $n$ získáme základní vzorec pro nekonečnou populaci:$$n \ge \frac{u_{1-\alpha/2}^2 p(1-p)}{\Delta^2 [f(Q_p)]^2}$$Pro konečnou populaci rozsahu $N$ musíme zahrnout korekci na konečný základní soubor ($1 - n/N$). Vzorec se pak modifikuje analogicky k rovnici (18) v práci Novákové 1:

$$n \ge \frac{1}{\frac{\Delta^2 [f(Q_p)]^2}{u_{1-\alpha/2}^2 p(1-p)} + \frac{1}{N}}$$
3.2 Problém "neznámé hustoty" při plánování
Zde narážíme na praktický paradox: abychom určili rozsah výběru, potřebujeme znát hodnotu hustoty $f(Q_p)$, kterou ale neznáme (proto děláme výběr). V semestrální práci musí být tento problém diskutován. Řešením je:
Pilotní studie: Odhadneme $f(Q_p)$ z malého předvýzkumu.
Parametrický předpoklad: Předpokládáme, že data pocházejí z určitého rozdělení (např. log-normálního), odhadneme jeho parametry a spočteme teoretickou hustotu.
V simulační části práce bude využit druhý přístup – jelikož generujeme data, známe teoretickou hustotu a můžeme ověřit, zda vypočtené $n$ skutečně vede k požadované přesnosti.
4. Specifikace simulační studie
Na základě požadavku uživatele ("demonstrovat vlastnosti na sešikmeném rozdělení") a inspirace prací Škrobánka 1 je navržen následující design experimentu.
4.1 Volba rozdělení: Log-normální model
Log-normální rozdělení je ideálním modelem pro ekonomické veličiny (mzdy, aktiva), které jsou typicky předmětem výběrových šetření ve statistické praxi.
Definice: Náhodná veličina $X$ má log-normální rozdělení $LN(\mu, \sigma)$, pokud $Y = \ln(X)$ má normální rozdělení $N(\mu, \sigma^2)$.
Parametry pro simulaci:
$\mu = 0$ (parametr polohy v log-škále).
$\sigma \in \{0.5, 1.0, 1.5\}$ (parametr tvaru). Zvýšení $\sigma$ dramaticky zvyšuje šikmost (skewness) a špičatost rozdělení. Práce by měla porovnat výsledky pro $\sigma=0.5$ (mírná šikmost) a $\sigma=1.5$ (extrémní šikmost).
4.2 Cílové kvantily
Simulace se nebude omezovat pouze na medián ($p=0.5$), který je u log-normálního rozdělení roven $e^\mu$. Kritickým testem Woodruffovy metody jsou chvostové kvantily, kde je hustota pravděpodobnosti nízká a zakřivení distribuční funkce vysoké.
Medián ($p=0.5$): Očekáváme dobré výsledky i pro menší rozsahy výběru.
Horní decil ($p=0.9$): Zde se projeví efekt sešikmení. Asymptotická normalita zde nastupuje pomaleji.
Extrémní kvantil ($p=0.95$ nebo $0.99$): Volitelně, pro demonstraci limitů metody.
4.3 Scénáře simulace
Pro každou kombinaci parametrů ($\sigma$, $p$) budou testovány různé rozsahy výběru $n$. V souladu s tabulkami v 1 a 1 navrhujeme:
$N = 100~000$ (velikost populace, fixní).
$n \in \{200, 500, 1000, 2000\}$.
4.4 Metriky hodnocení
Pro každé nastavení proběhne $R=1000$ (nebo více) replikací. Sledovány budou:
Relative Bias (Relativní zkreslení odhadu rozptylu): Nadhotnocuje nebo podhodnocuje Woodruffova metoda skutečný rozptyl?

$$RB = \frac{E - D_{True}}{D_{True}} \cdot 100\%$$

Kde $D_{True}$ získáme jako empirický rozptyl bodových odhadů kvantilů z Monte Carlo simulace.
Coverage Probability (Pravděpodobnost pokrytí): Kolik procent sestrojených 95% intervalů spolehlivosti skutečně pokrývá teoretický populační kvantil $Q_p$?
Očekáváme hodnotu blízkou 0.95. Odchylky (např. 0.92) budou indikovat selhání asymptotiky v důsledku šikmosti.
5. Implementace: Struktura kódu a algoritmizace
Pro AI agenta je nutné specifikovat nejen "co" počítat, ale "jak". Kód by měl být napsán v R (R Markdown), což je standard v kurzu 4ST414.
5.1 Algoritmus Woodruffovy metody (pseudokód)
Funkce WoodruffVariance(sample_data, p, alpha):
n = length(sample_data)
// 1. Spočti meze pro podíl
z_score = qnorm(1 - alpha/2)
margin = z_score * sqrt(p * (1-p) / n)
p_low = p - margin
p_high = p + margin
// 2. Ošetři hraniční indexy (aby nebyly < 1 nebo > n)
// Toto je důležitý detail pro robustní kód
idx_low = floor(p_low * n)
idx_high = ceiling(p_high * n)
// 3. Seřaď data
sorted_data = sort(sample_data)
// 4. Získej kvantily
q_low = sorted_data[idx_low]
q_high = sorted_data[idx_high]
// 5. Spočti Woodruffův interval a implikovaný SE
width = q_high - q_low
implied_se = width / (2 * z_score)
return implied_se^2
5.2 Struktura simulační smyčky
Kód musí obsahovat smyčku přes vektory parametrů sample_sizes a sigmas. Výsledky by měly být ukládány do data.frame a následně vizualizovány pomocí ggplot2. Důraz je kladen na reprodukovatelnost (nastavení set.seed).
6. Struktura cílového dokumentu (Semestrální práce)
Tato kapitola slouží jako přímá osnova pro AI agenta. Vygenerovaná práce musí striktně dodržet toto členění, aby odpovídala vzoru.1
Hlavička
Název: Využití Taylorova rozvoje (linearizace) pro odhad rozptylu výběrového kvantilu.
Autor: (Placeholder)
Předmět: 4ST414
Abstrakt: Stručné shrnutí (cca 150 slov) motivace, metodiky (Woodruff) a výsledků (vliv šikmosti).
Klíčová slova: Výběrový kvantil, Taylorova linearizace, Woodruffova metoda, Log-normální rozdělení.
JEL Classification: C10, C42, C83.
1. Úvod
Zasazení do kontextu: Proč nestačí průměry? (Odkaz na sociálně-ekonomické nerovnosti, kde medián lépe popisuje střední hodnotu než průměr).
Definice problému: Absence jednoduchého vzorce pro rozptyl kvantilu.
Cíl práce: Demonstrace a validace Woodruffovy metody na sešikmených datech.
2. Teoretická část
2.1 Definice kvantilů: Formální definice $F^{-1}(p)$.
2.2 Asymptotické vlastnosti: Uvedení Bahadurova vztahu a role hustoty $f(Q)$.
2.3 Taylorova linearizace: Podrobné odvození vztahu $(\hat{Q}-Q) \approx (p-\hat{F})/f$.
2.4 Woodruffova metoda: Popis "inverze intervalu" jako praktického řešení problému linearizace.
2.5 Stanovení rozsahu výběru: Odvození vzorce pro $n$ (viz kapitola 3.1 tohoto reportu).
3. Metodika a Data
Popis simulace: Zdůvodnění volby Log-normálního rozdělení.
Parametry: $N=100~000$, $\mu=0, \sigma=1$, $p=0.5, 0.9$.
Software: R verze X.Y.Z, knihovny sampling, ggplot2.
4. Výsledky (Analytická část)
4.1 Analýza potřebného rozsahu výběru: Tabulka (vzor Drážný 1) s vypočtenými teoretickými $n$ pro různé přesnosti $\Delta$. Diskuze o tom, jak dramaticky roste $n$ pro přesný odhad 90% kvantilu oproti mediánu.
4.2 Validace Woodruffovy metody: Tabulka (vzor Škrobánek 1) porovnávající pokrytí intervalů spolehlivosti.
Klíčový insight: U symetrických dat ($N(0,1)$ nebo $LN(0, 0.1)$) bude pokrytí cca 95%. U silně sešikmených dat ($LN(0, 1.5)$) a malých výběrů ($n=200$) bude pokrytí pravděpodobně nižší (tzv. undercoverage), protože linearizace podceňuje asymetrii výběrového rozdělení kvantilu.
4.3 Vizualizace: Histogramy odhadů kvantilů s vyznačeným Woodruffovým intervalem. QQ-ploty (vzor Nováková 1, Obrázek 2) ověřující normalitu odhadů.
5. Závěr
Syntéza zjištění: Woodruffova metoda je robustní pro mediány i u sešikmených dat, ale pro chvostové kvantily vyžaduje větší rozsahy výběru.
Odpověď na výzkumnou otázku z úvodu.
Seznam literatury
Cochran, W. G. (1977). Sampling Techniques.
Särndal, C.-E., Swensson, B., & Wretman, J. (1992). Model Assisted Survey Sampling.
Woodruff, R. S. (1952). Confidence intervals for medians and other position measures. Journal of the American Statistical Association.
Thompson, S. K. (2012). Sampling.
7. Syntéza a očekávané dopady pro výuku
Tento projekt není pouhým mechanickým cvičením. Student (a AI agent) na něm demonstruje pochopení hlubších souvislostí statistické teorie.
Odolnost vůči nelinearitě: Zatímco u průměru stačí znát rozptyl populace $S^2$, u kvantilu musíme znát tvar rozdělení (hustotu). Woodruffova metoda ukazuje, jak tuto informaci získat nepřímo z dat.
Vliv šikmosti: Log-normální simulace odhalí, že "univerzální" asymptotické vlastnosti nastupují u různých rozdělení různě rychle. Zatímco pro normální rozdělení stačí $n=30$, pro 99. percentil log-normálního rozdělení nemusí stačit ani $n=1000$.
Spojení teorie a praxe: Výpočet potřebného rozsahu výběru $n$ nutí studenta pracovat s teoretickou hustotou log-normálního rozdělení:

$$f(x) = \frac{1}{x\sigma\sqrt{2\pi}} e^{-\frac{(\ln x - \mu)^2}{2\sigma^2}}$$

Dosazení této funkce do vzorce pro $n$ je matematicky netriviální krok, který v pracích zaměřených pouze na průměry 1 chybí.
Závěrečné pokyny pro generování
Při generování finálního výstupu je nutné:
Zachovat striktní akademický tón ("bylo zjištěno", "je definováno").
Používat tabulky pro prezentaci všech numerických výsledků simulace.
Neuvádět pouze "úspěšné" výsledky, ale komentovat i případy, kdy metoda selhává (např. nízké pokrytí u malých vzorků v chvostu rozdělení), což prokazuje kritické myšlení vyžadované u zkoušky.
Zajistit, aby kód v R byl spustitelný "copy-paste" a obsahoval komentáře vysvětlující jednotlivé kroky linearizace.
Tento dokument představuje kompletní zadání. Agent by měl nyní postupovat sekci po sekci a vygenerovat text semestrální práce následovaný blokem kódu.
Citovaná díla
SchaffartzikovaSeminarka4ST413_fin_20200427.pdf
