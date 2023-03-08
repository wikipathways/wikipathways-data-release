# wikipathways-data-release
Monthly data release

## Purpose
This is a backup and future replacement for our wmcloud-hosted data archive site. In principle we should be able to replace that entire site and wp-data-release script with this repo (serves as Pages) and some GH Actions.

## Plan
We would serve that last 12 months of content (to keep the total size reasonable) on a data archive site just like the current one hosted here as GH Pages. We would also archive everything in Zenodo. We could then also include the Zenodo DOIs in the data archive lists to facilitate citations

We might explore hosting the site as a Jiffy (Jekyll + Wiki) site like our others, so that we could provide friendlier browsing and citation links, etc. The only requirement is that the site supports the legacy url patterns to directly download files, e.g., https://data.wikipathways.org/current/gmt/wikipathways-20230210-gmt-Sus_scrofa.gmt and https://data.wikipathways.org/20230210/gmt/wikipathways-20230210-gmt-Sus_scrofa.gmt. These are used by other tools and scripts by us and others.