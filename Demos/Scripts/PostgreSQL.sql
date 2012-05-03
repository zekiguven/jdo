create table jdo_demo (
	id serial not null,
	ftstr varchar(30),
	ftbool boolean,
	ftdate timestamp without time zone,
	ftfloat numeric,
	ftint integer,
	constraint pk_jdo_demo
		primary key (id)
);

create table jdo_demo_detail (
	id serial not null,
	ftstr varchar(30),
	jdodemoid integer not null,
	constraint pk_jdo_demo_detail
		primary key (id),
	constraint fk_jdo_demo_detail
		foreign key (jdodemoid)
		references jdo_demo (id)
		on update cascade
		on delete cascade
);
