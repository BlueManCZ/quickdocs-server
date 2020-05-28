-- MySQL dump 10.13  Distrib 8.0.20, for Linux (x86_64)
--
-- Host: localhost    Database: quickdocs
-- ------------------------------------------------------
-- Server version	8.0.20

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!50503 SET NAMES utf8mb4 */;
/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;
/*!40103 SET TIME_ZONE='+00:00' */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Table structure for table `cliki`
--

DROP TABLE IF EXISTS `cliki`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `cliki` (
  `project_name` varbinary(64) NOT NULL,
  `body` blob NOT NULL,
  `updated_at` bigint NOT NULL,
  UNIQUE KEY `project_name` (`project_name`)
) ENGINE=InnoDB DEFAULT CHARSET=binary;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `cliki`
--

LOCK TABLES `cliki` WRITE;
/*!40000 ALTER TABLE `cliki` DISABLE KEYS */;
/*!40000 ALTER TABLE `cliki` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `cliki_project_category`
--

DROP TABLE IF EXISTS `cliki_project_category`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `cliki_project_category` (
  `project_name` varbinary(64) NOT NULL,
  `category` varbinary(256) NOT NULL,
  UNIQUE KEY `project_name` (`project_name`,`category`),
  KEY `category` (`category`)
) ENGINE=InnoDB DEFAULT CHARSET=binary;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `cliki_project_category`
--

LOCK TABLES `cliki_project_category` WRITE;
/*!40000 ALTER TABLE `cliki_project_category` DISABLE KEYS */;
/*!40000 ALTER TABLE `cliki_project_category` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `examples`
--

DROP TABLE IF EXISTS `examples`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `examples` (
  `id` int NOT NULL AUTO_INCREMENT,
  `user_id` int NOT NULL,
  `project_name` varchar(40) NOT NULL,
  `project_system` varchar(40) NOT NULL,
  `project_package` varchar(40) NOT NULL,
  `project_symbol` varchar(40) NOT NULL,
  `markdown` text NOT NULL,
  `converted` text NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `examples`
--

LOCK TABLES `examples` WRITE;
/*!40000 ALTER TABLE `examples` DISABLE KEYS */;
/*!40000 ALTER TABLE `examples` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `preference`
--

DROP TABLE IF EXISTS `preference`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `preference` (
  `name` varchar(32) NOT NULL,
  `value` varchar(128) NOT NULL DEFAULT '',
  PRIMARY KEY (`name`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `preference`
--

LOCK TABLES `preference` WRITE;
/*!40000 ALTER TABLE `preference` DISABLE KEYS */;
INSERT INTO `preference` VALUES ('ql-dist-version','2020-03-25');
/*!40000 ALTER TABLE `preference` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `project`
--

DROP TABLE IF EXISTS `project`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `project` (
  `id` bigint unsigned NOT NULL AUTO_INCREMENT,
  `ql_dist_version` binary(10) NOT NULL,
  `name` varbinary(64) NOT NULL,
  `release_version` binary(10) NOT NULL,
  `homepage_url` tinyblob,
  `repos_url` tinyblob,
  `archive_url` tinyblob NOT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `ql_dist_version` (`ql_dist_version`,`name`),
  KEY `name` (`name`)
) ENGINE=InnoDB AUTO_INCREMENT=259 DEFAULT CHARSET=binary;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `project`
--

LOCK TABLES `project` WRITE;
/*!40000 ALTER TABLE `project` DISABLE KEYS */;
/*!40000 ALTER TABLE `project` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `project_readme`
--

DROP TABLE IF EXISTS `project_readme`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `project_readme` (
  `project_id` bigint unsigned NOT NULL,
  `filename` tinytext,
  `raw` text,
  `converted` text,
  KEY `project_id` (`project_id`),
  CONSTRAINT `project_readme_ibfk_1` FOREIGN KEY (`project_id`) REFERENCES `project` (`id`) ON DELETE CASCADE ON UPDATE RESTRICT
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `project_readme`
--

LOCK TABLES `project_readme` WRITE;
/*!40000 ALTER TABLE `project_readme` DISABLE KEYS */;
/*!40000 ALTER TABLE `project_readme` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `quicklisp_download_stats`
--

DROP TABLE IF EXISTS `quicklisp_download_stats`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `quicklisp_download_stats` (
  `project_name` varbinary(64) NOT NULL,
  `download_count` int NOT NULL,
  UNIQUE KEY `project_name` (`project_name`)
) ENGINE=InnoDB DEFAULT CHARSET=binary;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `quicklisp_download_stats`
--

LOCK TABLES `quicklisp_download_stats` WRITE;
/*!40000 ALTER TABLE `quicklisp_download_stats` DISABLE KEYS */;
/*!40000 ALTER TABLE `quicklisp_download_stats` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `repos_info`
--

DROP TABLE IF EXISTS `repos_info`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `repos_info` (
  `project_name` varbinary(64) NOT NULL,
  `type` enum('github','bitbucket') NOT NULL,
  `repos_id` tinyblob NOT NULL,
  `description` text CHARACTER SET utf8 COLLATE utf8_general_ci,
  `homepage_url` tinyblob,
  `watch_count` int NOT NULL,
  `forks_count` int NOT NULL,
  `stars_count` int DEFAULT NULL,
  `created_at` int NOT NULL,
  `updated_at` bigint NOT NULL,
  UNIQUE KEY `project_name` (`project_name`)
) ENGINE=InnoDB DEFAULT CHARSET=binary;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `repos_info`
--

LOCK TABLES `repos_info` WRITE;
/*!40000 ALTER TABLE `repos_info` DISABLE KEYS */;
/*!40000 ALTER TABLE `repos_info` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `system`
--

DROP TABLE IF EXISTS `system`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `system` (
  `id` bigint unsigned NOT NULL AUTO_INCREMENT,
  `project_id` bigint unsigned NOT NULL,
  `name` varbinary(64) NOT NULL,
  `version` varbinary(32) DEFAULT NULL,
  `description` blob,
  `long_description` blob,
  `homepage_url` tinyblob,
  `license` blob,
  PRIMARY KEY (`id`),
  UNIQUE KEY `project_id` (`project_id`,`name`),
  CONSTRAINT `system_ibfk_1` FOREIGN KEY (`project_id`) REFERENCES `project` (`id`) ON DELETE CASCADE ON UPDATE RESTRICT
) ENGINE=InnoDB AUTO_INCREMENT=473 DEFAULT CHARSET=binary;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `system`
--

LOCK TABLES `system` WRITE;
/*!40000 ALTER TABLE `system` DISABLE KEYS */;
/*!40000 ALTER TABLE `system` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `system_author`
--

DROP TABLE IF EXISTS `system_author`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `system_author` (
  `id` bigint unsigned NOT NULL AUTO_INCREMENT,
  `system_id` bigint unsigned NOT NULL,
  `author_name` varbinary(256) NOT NULL,
  `type` enum('author','maintainer') NOT NULL DEFAULT 'author',
  PRIMARY KEY (`id`),
  KEY `system_id` (`system_id`),
  CONSTRAINT `system_author_ibfk_1` FOREIGN KEY (`system_id`) REFERENCES `system` (`id`) ON DELETE CASCADE ON UPDATE RESTRICT
) ENGINE=InnoDB DEFAULT CHARSET=binary;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `system_author`
--

LOCK TABLES `system_author` WRITE;
/*!40000 ALTER TABLE `system_author` DISABLE KEYS */;
/*!40000 ALTER TABLE `system_author` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `system_dependencies`
--

DROP TABLE IF EXISTS `system_dependencies`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `system_dependencies` (
  `id` bigint unsigned NOT NULL AUTO_INCREMENT,
  `system_id` bigint unsigned NOT NULL,
  `depends_system_id` bigint unsigned NOT NULL,
  `is_for_defsystem` tinyint NOT NULL DEFAULT '0',
  PRIMARY KEY (`id`),
  UNIQUE KEY `system_id` (`system_id`,`depends_system_id`,`is_for_defsystem`),
  KEY `depends_system_id` (`depends_system_id`),
  CONSTRAINT `system_dependencies_ibfk_1` FOREIGN KEY (`system_id`) REFERENCES `system` (`id`) ON DELETE CASCADE ON UPDATE RESTRICT,
  CONSTRAINT `system_dependencies_ibfk_2` FOREIGN KEY (`depends_system_id`) REFERENCES `system` (`id`) ON DELETE CASCADE ON UPDATE RESTRICT
) ENGINE=InnoDB DEFAULT CHARSET=binary;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `system_dependencies`
--

LOCK TABLES `system_dependencies` WRITE;
/*!40000 ALTER TABLE `system_dependencies` DISABLE KEYS */;
/*!40000 ALTER TABLE `system_dependencies` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `system_extracted_info`
--

DROP TABLE IF EXISTS `system_extracted_info`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `system_extracted_info` (
  `system_id` bigint unsigned NOT NULL,
  `packages` longtext NOT NULL,
  `failed` tinyint NOT NULL DEFAULT '0',
  `error_log` text NOT NULL,
  UNIQUE KEY `system_id` (`system_id`),
  CONSTRAINT `system_extracted_info_ibfk_1` FOREIGN KEY (`system_id`) REFERENCES `system` (`id`) ON DELETE CASCADE ON UPDATE RESTRICT
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `system_extracted_info`
--

LOCK TABLES `system_extracted_info` WRITE;
/*!40000 ALTER TABLE `system_extracted_info` DISABLE KEYS */;
/*!40000 ALTER TABLE `system_extracted_info` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `users`
--

DROP TABLE IF EXISTS `users`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `users` (
  `id` int NOT NULL AUTO_INCREMENT,
  `service` int NOT NULL,
  `service_id` int DEFAULT NULL,
  `login` varchar(40) NOT NULL,
  `user_name` varchar(80) DEFAULT NULL,
  `html_url` varchar(80) DEFAULT NULL,
  `avatar_url` varchar(80) DEFAULT NULL,
  `company` varchar(80) DEFAULT NULL,
  `location` varchar(20) DEFAULT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `users_id_uindex` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `users`
--

LOCK TABLES `users` WRITE;
/*!40000 ALTER TABLE `users` DISABLE KEYS */;
/*!40000 ALTER TABLE `users` ENABLE KEYS */;
UNLOCK TABLES;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2020-05-28 11:26:33
