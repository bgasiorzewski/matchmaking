-- createuser -U postgres matchmaking
-- createdb -U postgres matchmaking
-- psql -U postgres -d matchmaking -f matchmaking.sql

GRANT CONNECT ON DATABASE matchmaking TO matchmaking;

CREATE TABLE matches (
    hotslogs_match BIGINT PRIMARY KEY,
    time_played TIMESTAMP NOT NULL,
    mmr_high INTEGER NOT NULL,
    mmr_low INTEGER NOT NULL,
    name_high VARCHAR(12) NOT NULL,
    name_low VARCHAR(12) NOT NULL,
    hotslogs_region INTEGER NOT NULL
);

CREATE INDEX matches_spread ON matches (time_played, (mmr_high - mmr_low));

GRANT SELECT, INSERT ON matches TO matchmaking;
