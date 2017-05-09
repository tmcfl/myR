
# METHOD 1:

## Get currently installed packages (from after R update)
tmp <- as.data.frame(installed.packages())
tmp_list <- tmp$Package

## Get previously installed packages
package_df <- as.data.frame(installed.packages("/Library/Frameworks/R.framework/Versions/3.3/Resources/library"))
package_list <- as.character(package_df$Package)
package_list <- package_list[!package_list %in% tmp_list]

## Re-install Install packages
install.packages(package_list)

tmp2 <- as.data.frame(installed.packages())
tmp2_list <- tmp2$Package

github_list <- package_df$Package[is.na(package_df$NeedsCompilation) & !package_df$Priority %in% c("base", "recommended")]
github_list <- github_list[!github_list %in% tmp2$Package]



# METHOD 2:

## More elegent method?
update.packages(ask=FALSE, checkBuilt=TRUE)
