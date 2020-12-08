# lil-regy -f "<head prefix=\"" -r "<head prefix=\"sc: http://schema.org " -d -p /Users/jfriesen/git/takeonrules.github.io/
# lil-regy -f "<html" -r "<html xmlns=\"http://www.w3.org/1999/xhtml\" xmlns:sc=\"http://schema.org\"" -d -p  /Users/jfriesen/git/takeonrules.github.io/
lil-regy -f "itemid=\"\{\{[^\}]+\}\}#skip-to-content\"" -r "itemid=\"#skip-to-content\"" -d -p /Users/jfriesen/git/takeonrules.github.io/
# lil-regy -f "article: http://ogp.me/ns/article#" -r "art: http://ogp.me/ns/article#" -d -p /Users/jfriesen/git/takeonrules.github.io/
# lil-regy -f "\"article:" -r "\"art:" -d -p /Users/jfriesen/git/takeonrules.github.io/
lil-regy -f " itemscope itemtype=\"http://schema.org/" -r " typeof=\"sc:" -d -p /Users/jfriesen/git/takeonrules.github.io/
lil-regy -f " itemprop=\"" -r " property=\"sc:" -d -p /Users/jfriesen/git/takeonrules.github.io/
lil-regy -f " itemid=" -r " resource=" -d -p /Users/jfriesen/git/takeonrules.github.io/
# lil-regy -f "<body" -r "<body prefix=\"sc: http://schema.org/\"" -p /Users/jfriesen/git/takeonrules.github.io/themes/hugo-tufte/layouts/_default/baseof.html -d