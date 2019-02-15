<?php
	execute("
		DROP TABLE IF EXISTS `code`;
		CREATE TABLE `code` (		
			`id` 		INTEGER  	 NOT NULL DEFAULT 0  COMMENT 'id',
			`sign` 		VARCHAR(32)  NOT NULL DEFAULT '' COMMENT '英文描述',
			`desc` 		VARCHAR(128)  NOT NULL DEFAULT '' COMMENT '中文描述',
			PRIMARY KEY (`id`)
		) ENGINE=InnoDB  DEFAULT CHARSET=utf8 COMMENT='返回码表';
	");
?>