class Query (strs: Seq[String]) extends Weighted[String] {
	var wordAndWeight: Seq[(String, Double)] = Seq[(String, Double)]()

	def getItems(): Seq[String] = {
		strs
	}

	def getWeights(): Seq[Double] = {
		assignWeights()
		val ret = for(weight <- wordAndWeight) yield weight._2
		ret
	}

	def assignWeights(): Unit = {
		wordAndWeight = strs.map((_, 1.0))
	}
}

class DictionaryQuery(strs: Seq[String]) extends Query(strs: Seq[String]) {


	var badString: String = "a, hence, see, able, her, seeing, about, here, seem, above, hereafter, seemed, abroad, hereby, seeming, according, herein, seems, accordingly, here's, seen, across, hereupon, self, actually, hers, selves, adj, herself, sensible, after, he's, sent, afterwards, hi, serious, again, him, seriously, against, himself, seven, ago, his, several, ahead, hither, shall, ain't, hopefully, shan't, all, how, she, allow, howbeit, she'd, allows, however, she'll, almost, hundred, she's, alone, i, should, along, i'd, shouldn't, alongside, ie, since, already, if, six, also, ignored, so, although, i'll, some, always, i'm, somebody, am, immediate, someday, amid, in, somehow, amidst, inasmuch, someone, among, inc, something, amongst, inc., sometime, an, indeed, sometimes, and, indicate, somewhat, another, indicated, somewhere, any, indicates, soon, anybody, inner, sorry, anyhow, inside, specified, anyone, insofar, specify, anything, instead, specifying, anyway, into, still, anyways, inward, sub, anywhere, is, such, apart, isn't, sup, appear, it, sure, appreciate, it'd, t, appropriate, it'll, take, are, its, taken, aren't, it's, taking, around, itself, tell, as, i've, tends, a's, j, th, aside, just, than, ask, k, thank, asking, keep, thanks, associated, keeps, thanx, at, kept, that, available, know, that'll, away, known, thats, awfully, knows, that's, b, l, that've, back, last, the, backward, lately, their, backwards, later, theirs, be, latter, them, became, latterly, themselves, because, least, then, become, less, thence, becomes, lest, there, becoming, let, thereafter, been, let's, thereby, before, like, there'd, beforehand, liked, therefore, begin, likely, therein, behind, likewise, there'll, being, little, there're, believe, look, theres, below, looking, there's, beside, looks, thereupon, besides, low, there've, best, lower, these, better, ltd, they, between, m, they'd, beyond, made, they'll, both, mainly, they're, brief, make, they've, but, makes, thing, by, many, things, c, may, think, came, maybe, third, can, mayn't, thirty, cannot, me, this, cant, mean, thorough, can't, meantime, thoroughly, caption, meanwhile, those, cause, merely, though, causes, might, three, certain, mightn't, through, certainly, mine, throughout, changes, minus, thru, clearly, miss, thus, c'mon, more, till, co, moreover, to, co., most, together, com, mostly, too, come, mr, took, comes, mrs, toward, concerning, much, towards, consequently, must, tried, consider, mustn't, tries, considering, my, truly, contain, myself, try, containing, n, trying, contains, name, t's, corresponding, namely, twice, could, nd, two, couldn't, near, u, course, nearly, un, c's, necessary, under, currently, need, underneath, d, needn't, undoing, dare, needs, unfortunately, daren't, neither, unless, definitely, never, unlike, described, neverf, unlikely, despite, neverless, until, did, nevertheless, unto, didn't, new, up, different, next, upon, directly, nine, upwards, do, ninety, us, does, no, use, doesn't, nobody, used, doing, non, useful, done, none, uses, don't, nonetheless, using, down, noone, usually, downwards, no-one, v, during, nor, value, e, normally, various, each, not, versus, edu, nothing, very, eg, notwithstanding, via, eight, novel, viz, eighty, now, vs, either, nowhere, w, else, o, want, elsewhere, obviously, wants, end, of, was, ending, off, wasn't, enough, often, way, entirely, oh, we, especially, ok, we'd, et, okay, welcome, etc, old, well, even, on, we'll, ever, once, went, evermore, one, were, every, ones, we're, everybody, one's, weren't, everyone, only, we've, everything, onto, what, everywhere, opposite, whatever, ex, or, what'll, exactly, other, what's, example, others, what've, except, otherwise, when, f, ought, whence, fairly, oughtn't, whenever, far, our, where, farther, ours, whereafter, few, ourselves, whereas, fewer, out, whereby, fifth, outside, wherein, first, over, where's, five, overall, whereupon, followed, own, wherever, following, p, whether, follows, particular, which, for, particularly, whichever, forever, past, while, former, per, whilst, formerly, perhaps, whither, forth, placed, who, forward, please, who'd, found, plus, whoever, four, possible, whole, from, presumably, who'll, further, probably, whom, furthermore, provided, whomever, g, provides, who's, get, q, whose, gets, que, why, getting, quite, will, given, qv, willing, gives, r, wish, go, rather, with, goes, rd, within, going, re, without, gone, really, wonder, got, reasonably, won't, gotten, recent, would, greetings, recently, wouldn't, h, regarding, x, had, regardless, y, hadn't, regards, yes, half, relatively, yet, happens, respectively, you, hardly, right, you'd, has, round, you'll, hasn't, s, your, have, said, you're, haven't, same, yours, having, saw, yourself, he, say, yourselves, he'd, saying, you've, he'll, says, z, hello, second, zero, help, secondly"
	var goodString: String = "facebook, youtube, google, gmail, hotmail, xxnx, xvideos, amazon, translator, xxx, pornos, facebook, login, yahoo, google, translate, yahoomail, google, maps, ebay, whatsapp, porn, instagram, traductor, weather, redtube, netflix, outlook, pokemon, go, twitter, xhamster, fahrenheit, youporn, olx, craigslist, msn, grammes, free, porn, beeg, youtube, mp3, snapchat, you, messenger, news, sex, videos, pornotube, ali, express, walmart, bbc, news, google, trad, clash, of, clans, linkedin, tubidy, pinterest, vminecraft, glob, whatsapp, web, mincraft, badoo, bideo, porno, video, porn, ikea, live, score, cnn, cable, news, network, e, okul, youtube, to, mp3, olympics, auol, uolo, speed, test, speedometer, test, minecraft, traduttore, game, games, paypal, tiempos, daily, mail, pornhub, crikbuzz, sahibinden, MercadoLibre, ok, google, libero, mail, milliyet, bild, marca, marcar, google, tradutor, euro, 2016, google, onedrive, ryanair, traduttor, myn, skype, aleg, espn, ebay, kleinanzeigen, factory, automation, football, association, mail, mailen, mails, nynet, beezars, allegro, convertidos, sexuality, video, wp, sex, videos, flipkart, home, depot, home-depot, hotel, hotelaria, restaurant, restauration, fox, news, iphone, 7, chaturbate, ebay, uk, national, basketball, association, nba, g, drive, google, drive, bet365, pokemom, after, service, akciova, spolecnost, dropbox, bed, 365, bookers, booking, zalando, oranges, calcu, omegle, indeed, calculator, calculators, mp3, pandora, target, zara, airbnb, best, buy, brazzers, irctc, cricbuzz, uc, browser, phim, sex, gmx, satta, matka, tubemate, traductor, google, juegos, hotels, putlocker, donald, trump, patriot, ordnance, factory, pof, restaurants, bbc, sports, wells, fargo, pirate, bay, tumblr, office, 365, office365, youtube, converter, goggle, docs, billionaire, boys, club, bbc, wikipedia, el, mundo, traduzir, cam4, leboncoin, pizza, hut, pizzahut, deals, santander, hentai, el, pais, subway, surfers, brazzer, brazzers, roblox, soundcloud, bank, of, america, google, play, gum, tree, spotify, dominos, interia, goggle, chrome, staeam, steam, international, mathematics, olympiad, playstore, apple, groupon, itube.com, amazon, uk, amazon, united, kingdom, cricinfo, argos, atgis, augos, tube8, xxx, video, facebook, lite, adna, klasnik, odnoklassniki, imdb, aol, mail, aol, mailbox, sports, man, united, justin, bieber, state, bank, india, online, american, online, inc, sbi, online, google, docs, backpages, lowes, cosco, aol, bing, wwe, anti, virus, dich, hdfc, netbanking, auto, trader, candy, crush, perfect, girl, expedia, vodafone, adidas, porn, tube, pornstar, tube, kooora, trivago, liber, nike, viber, google, chrome, chrome, chromed, trip, advisor, automated, surface, observing, system, whatsapp, messenger, line, macys, skyscanner, facebook, en, espa, ol, decathlon, shareit, snap, deal, snapdeal, 24h, internet, movie, database, banco, de, venezuela, reddit, icloud, uber, trump, game, of, thrones, aright, move, peppa, pig, deadpool, national, football, league, nfl, samsung, world, star, hip, hop, aloewishes, anime, goog, mp3xd, lotto, meenakshisundaram, metros, monsieur, alvyda, american, airlines, ansaa, flashscore, real, madrid, itunes, accuweather, yandex, youtube, downloader, capital, one, dailymotion, hsbc, email, pages, jaunes, google, earth, mcdonalds, asck, clean, master, amazon, prime, ask, imo, porno, gratis, pornogratis, usps, tracking, zillows, kohls, paytm, gta, costco, facebook, messenger, happy, wheels, kim, kardashian, porn, video, toys, r, us, cricket, scoreboard, cricket, scores, traducir, mercado, libre, mexican, restaurants, near, me, nfl, scores, dravi, star, wars, google, scholar, cooi, math, gams, cool, math, game, solitaire, afton, bladet, google, news, google, images, tinder, chase, goggle, images, caf, iltalehti, vpn, goggle, news, iphone, 6s, selena, gomez, mp3, converter, aresanel, driver, suicide, squad, mobile, mobili, wallpapers, wc, forever, 21, powerball, ticketmaster, web, whatsapp, a, erocam, aor, omes, asrnel, bbc, weather, hotmaillogin, corrosion, of, conformity, windows, 10, easyjet, hot, litre, mapquest, vn, express, kizi, youtube, videos, bradesco, lol, clarin, photo, edit, photography, editing, trenitalia, candy, crush, saga, fanatik, mappy, b6, 12, meteos, celsius, firefox, grand, theft, auto, greater, toronto, area, kik, next, victoria, secret, sexo, teamviewer, etsy, bla, bla, car, indian, railway, lazada, pnr-status, hotstar, x, video, bidmate, jogos, mfacebook, la, caixa, t-mobile, hillary, clinton, restaurant, near, me, ymz, tmz, Ariana, Grande, sky, news, mp3, juice, ig, cool, math, the, walking, dead, ilta-sanomat, arsenal, asana, barca, barcelona, barcelonistas, sky, sports, ultimate, fighting, championship, taylor, swift, banisteriopsis, general, motors, genetically, modified, gm, le, monde, web, kurir, chased, prince, 123movies, kayak, b, lic, minion, music, downloader, youtube, music, edmodo, cars, hola, att, happy, birthday, dee, iphone, 6, wish, papa, johns, sat, major, league, baseball, mlb, australian, broadcasting, corporation, index, indexer, credit, karma, snap, tube, aloha, tube, banco, estado, kickass, aphotoshop, abc, fifa, 16, db, deutsche, bahn, iphone, 5s, actualitees, messi, kylie, jenner, utorrent, image, terra, google, classrooms, rediffmail, torrents, extra, torrent, xdj, starbucks, movies, matka, wordreference, anime, sarkari, result, retrica, 4shared, kahoot, habe, ups, track, 9, gag, antozone, auroson, auroxone, gazeteler, tesco, bank2u, downloads, mango, ok, qvc, unicredit, banco, bilbao, vizcaya, argentaria, bbvas, horoscope, 24, sata, baqnesco, credit, agricole, bed, bath, and, beyond, buienradar, old, navy, arefour, dictionaries, dictionary, foot, locker, footlocker, j.c., penney, pnr, status, amerikano, aricanas, gamestop, nordstrom, bookmyshow, liverpool, rambler, major, league, baseball, scores, mlb, scores, nba, scores, the, pirate, bay, golden, state, warriors, onedrive, cheap, flights, vertalen, cheapest, flights, citi, bank, outlook, 365, chase, online, watch, series, david, bowie, google, flights, bbc, football, united, airlines, bernie, sanders, testing, tests, toyota, webmail, ajva, fitbit, java, american, express, gay, porn, ivanovic, television, guide, tv, guide, cristiano, ronaldo, rihanna, southwest, airlines, sw, airlines, java, -djava.security.policy, oppure, ovvero, spider, man, online, sbi, bmw, music, ynet, dragon, ball, super, iphones, anerucab, exoress, verizon, icicinet, banking, roku, popcorn, time, pornhd, delfi, sabah, hello, eme, winrar, youtubes, music, n, united, states, post, office, east, premier, league, piano, skies, sky, mp3, downloader, asda, geometry, dash, foot, football, flashlight, mobile, de, tubegalore, photoshop, ps4, 360, security, thesaurus, ups, free, sex, uninterrupted, power, supply, adobe, reader, subway, league, of, legends, movistar, guardian, guardians, fedex, gmail, login, applock, naruto, apps, grand, theft, auto, 5, o2, anemei, currency, converter, emoji, walgreen, john, lewis, milf, wechat, hd, porn, download, moto, gp, sears, autozone, bideos, porno, gratis, huffington, post, porn, hd, telecinco, chatro, chatroulet, la, repubblica, photo, grid, animal, jam, apple, store, avitop, palmeiras, aldis, candy, camera, christian, outreach, centre, segunda, mano, videos, porno, gratis, bonprix, carrefour, cinemark, new, look, times, of, india, truecaller, antera, vasna, fnac, live, cricket, score, live, cricket, scoreboard, program, tv, netbanking, conor, mcgregor, comcast, mail, online, halifax, adobe, flash, player, coco, teen, porn, pubmed, honda, lottery, plenty, of, fish, a, bola, bee, bee, expressing, expression, lloyds, amel, amelie, adele, frozen, people, video, one, hyundai, spiegel, captain, america, civil, war, kendall, jenner, national, hockey, league, federation, internationale, de, football, association, fifa, natwest, beyonce, dragon, city, drudge, report, iphone, se, nudevista, fandango, google-analytics, bod, harry, potter, champions, league, us, bank, microsoft, mundo, deportivo, fc, barcelona, hay, day, chelsea, le, figaro, mp3, music, downloading, xbox, one, ford, volkswagen, angelina, jolie, leos, el, confidencial, michael, kors, translate, google, british, airways, eurosport, afipo, la, nacion, mega, warrior, jota, record, b, anco, do, brasil, fallout, 4, nicki, minaj, pac-man, mercedes, rojadirecta, slither, societe, generale, avob, avon, mia, khalifa, one, piece, fifa, 15, booking.com, jav, anco, santander, chromecast, lego, cartolas, telegram, radio, sex, tube, myfreecams, shazam, sudoku, asds, jeux, urban, dictionary, deezer, marks, and, spencer, air, france, cars, game, staple, video, downloader, acura, games, camera, 360, car, g, car, game, la, poste, vlc, abast, airasia, mahjong, paris, pdf, porno, xxx, pornografia, xxx, pornos, xxx, portable, document, format, temple, run, 2, avast, avsst, dainikbhaskar, fedex, track, fedex, tracking, porn, movies, porno, movies, whatsapp, download, 9apps, k, mart, launcher, otto, laredoute, serie, a, conforama, debenhams, kmart, white, pages, whitepages, meetic, cinepolis, lojas, americanas, redbox, bershka, garanti, mx, player, tokopedia, zalo, viamichelin, correios, tredyol, cargams, traductor, de, google, yahoo, finance, johnny, depp, leonardo, dicaprio, ok, cupid, buzzfeed, bancoombia, pizza, george, boole, corriere, expressen, anime, flv, aprint, earth, day, periscope, spanish, to, english, xfinity, rezultati, asprint, william, hill, william, shakespeare, nordea, sprint, scholar, scholarship, barclay, barclays, ask.fm, national, lottery, tiscali, cub, el, comercio, isis, samsung, galaxy, s7, audi, blogger, el, universal, the, revenant, quot, quotes, rio, 2016, google, analytics, halloween, miniclip, western, union, katy, perry, one, direction, a, naked, girl, boo, hoo, focus, neymar, banco, provincial, yelp, delta, drake, film, streaming, nissan, batman, hoverboard, quizlet, snap, yahoo, news, british, telecom, brad, pitt, copa, america, juventus, sonic, bekmen, tesla, avianca, google, calendar, verizon, wireless, sex, stories, hangout, jworg, barbie, bundesliga, chatting, moneycontrol, television, online, meme, photo, free, games, free, gaming, freeware, games, need, for, speed, cricket, harley, quinn, provincial, michael, jackson, tango, vivo, goggle, mail, cartoon, network, paris, saint-germain, burger, king, camera, comuniazo, primark, angry, birds, alibaba, boot, global, positioning, system, office, depot, batman, vs, superman, dragon, ball, z, party, city, banki, adblock, euro, imgur, akinator, curries, curry, flash, player, google, mail, opera, piano, tiles, planetromeo, banco, do, brasil, kfc, bancosantander, chile, keyboard, pacman, 8ball, pool, bnp, british, national, party, el, corte, ingles, f1, zedge, app, lock, applications, lock, free, porn, videos, jennifer, lawrence, literot, literotica, louis, vuitton, river, island, sex, xxx, what, bluestacks, galinha, pintadinha, gossip, margot, robbie, miley, cyrus, barnes, and, noble, gitt, make, my, trip, michael, pegasus, plants, vs, zombies, hobby, lobby, waze, canadian, tire, e-mag, american, eagle, gta, san, andreas, dubsmash, falabella, gazeta, stradivarius, uc, mini, urban, outfitters, zing, extras, casto, rotten, tomatoes, stephen, curry, club, penguin, mlb, standings, cat, kanye, west, cici, scratch, zeta, national, rail, train, running, status, vikings, time, free, mobile, adobe, naked, girl, fabswingers, flickr, transformers, english, soccer, premier, league, mac, kickers, shemale, enterprise, songs, emoji, keyboard, tetris, mario, live, television, lufthansa, vivastreet, gaps, mobo, market, dick, sporting, goods, nude, sexy, trade, me, scotiabank, livecricket, shopclues, american, eagle, submarino, elmundo, chatroulette, flower, internet"


	var goodDictionary: Seq[String] = Seq[String]()
	var badDictionary: Seq[String] = Seq[String]()

	var superWordAndWeight: Seq[(String, Double)] = Seq[(String, Double)]()
	var wAndWeight: Seq[(String, Double)] = Seq[(String, Double)]()

	override def getWeights(): Seq[Double] = {
		if (wAndWeight.length < strs.length){
			loadGood()
			loadBad()
			assignWeights()
		}
		
		val ret = for(weight <- wAndWeight) yield weight._2
		ret
	}

	def loadGood(): Seq[String] = {
		if (goodDictionary.length > 0){
			goodDictionary
		}

		val words = goodString.split(",")
		for (w <- words){
			goodDictionary = goodDictionary :+ w
		}
		
		goodDictionary
	}

	def loadBad(): Seq[String] = {
		if (badDictionary.length > 0){
			badDictionary
		}

		val words = badString.split(",")
		for (w <- words){
			badDictionary = badDictionary :+ w
		}
		
		badDictionary


	}

	override def assignWeights(): Unit = {
		if (strs.length > wAndWeight.length){
			superWordAndWeight = strs.map((_, 4.0))
		
			for (s <- superWordAndWeight){
                if (goodDictionary.contains(s._1)){
                    wAndWeight = wAndWeight :+ (s._1, 10.0)
                }
                else if (badDictionary.contains(s._1)){
                    wAndWeight = wAndWeight :+ (s._1, 1.0)
                }
                else{
                    wAndWeight = wAndWeight :+ (s._1, 3.0)
                }
            }

			print("This is my Size: "+wAndWeight.length)
		}
	}
}
