
# METHOD 1:

## Get currently installed packages
package_df <- as.data.frame(installed.packages("/Library/Frameworks/R.framework/Versions/3.2/Resources/library"))
package_list <- as.character(package_df$Package)

## Re-install Install packages
install.packages(package_list)

# METHOD 2:

## More elegent method?
update.packages(ask=FALSE, checkBuilt=TRUE)