create table VEHICLE (PLATE CHAR(8) NOT NULL, MAKE VARCHAR(20), PRICE INTEGER, OWNER CHAR(20)  );
insert into VEHICLE values ('E265 FOO', 'VAUXHALL', 5000, '');
insert into VEHICLE values ('XDG 792S', 'ROLLS', 50000, '');
insert into VEHICLE values ('F360 OOL', 'FORD', 4000, 'PERSEPHONE');
insert into VEHICLE values ('H151 EEE', 'JAGUAR', 15000, '');
insert into VEHICLE values ('G722 HAD', 'SKODA', 500, '');

create table PERSON (NAME CHAR(20) NOT NULL, SALARY INTEGER, VEHICLE CHAR(8), EMPLOYER CHAR(20)  ) ;
insert into PERSON values ('FRED', 10000, '', 'IBM');
insert into PERSON values ('HARRY', 20000, '', 'FORD');
insert into PERSON values ('PHOEBE', 5000, '', '' );
insert into PERSON values ('TOM', 50000, '', 'ACME' );
insert into PERSON values ('PERSEPHONE', 15000, 'F360 OOL', 'ICL');

create table COMPANY (NAME CHAR (20), PRODUCT CHAR(10) );
insert into COMPANY values ('IBM', 'COMPUTERS');
insert into COMPANY values ('FORD', 'CARS');
insert into COMPANY values ('ICL', 'COMPUTERS');
insert into COMPANY values ('ACME', 'TEAPOTS');
