# Is Hero League matchmaking for Heroes of the Storm fixed yet?

Visit [www.IsMatchmakingFixedYet.com](http://www.ismatchmakingfixedyet.com/) to find out!

Shoutout to Blizzard for creating [Heroes of the Storm](http://www.heroesofthestorm.com/), and to Ben Barrett for creating [Hotslogs](http://www.hotslogs.com/)!

## How does it work?

There is a scraper running in the background that continuously checks Hotslogs for new matches played by the top 100 players from NA and EU. Each new match is added to the database. Then, the background service calculates the percentage of matches played within the last 24 hours, in which the difference between the highest and lowest-ranked player is over 1000 Hotslogs MMR.

The ultimate answer is "YES" if that percentage is less than 50%. Otherwise the answer is "NO."

## What is the tech behind it?

The website is written in [Haskell](https://www.haskell.org/).

It uses:

- [http-client](https://hackage.haskell.org/package/http-client) and [tagsoup](https://hackage.haskell.org/package/tagsoup) to gently scrape Hotslogs (if ismatchmakingfixedyet.com suddenly stops updating with new matches, it's because of my fragile tagsoup parsing)
- [scotty](https://hackage.haskell.org/package/scotty) and [blaze](https://hackage.haskell.org/package/blaze-html) to display the beautiful web page
- [ekg](https://hackage.haskell.org/package/ekg) for "monitoring" (visit the website on port 8000 to see it in action)
- [postgresql-simple](https://hackage.haskell.org/package/postgresql-simple) (and [PostgreSQL](http://www.postgresql.org/)) for data storage
- [nginx](http://nginx.org/) as a reverse proxy and for static files
- [stack](http://www.haskellstack.org/) for building

---

Copyright 2016 Bartek Gąsiorzewski
