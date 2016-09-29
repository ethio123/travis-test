# DC schema
 
# --- !Ups

CREATE TABLE _addr (
  id SERIAL NOT NULL PRIMARY KEY,
  user_id VARCHAR(31) NOT NULL,
  json VARCHAR(8191) NOT NULL
);

# --- !Downs


DROP TABLE _addr;
