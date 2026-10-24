CREATE TABLE IF NOT EXISTS rules_embeds (
    guild_id BIGINT NOT NULL,
    channel_id BIGINT NOT NULL,
    message_id BIGINT NOT NULL,
    PRIMARY KEY (guild_id)
);