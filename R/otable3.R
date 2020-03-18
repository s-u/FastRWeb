otable3 <- function (..., cb.title="table", cb.id="cb_bla", cb.divid=sprintf("%s_div",cb.id)) 
# Wrapper of otable2, but format as show/hide via checkbox. Uses javascript for the toggling of the view.
# Include the following JS function in the html page:
# function togviz(id) {
#  var el = document.getElementById(id);
#  if (el) {
#    if (el.style.display=='none')
#      el.style.display='inline';
#    else
#      el.style.display='none';
#  }
#  true;
# }
# Example usage: otable3(data.frame(list(a=1:3,b=2:4)))

{
	out(sprintf(
		"<input type=\"checkbox\" onclick=\"%s\"/>%s<br/>",
		sprintf( "document.getElementById('%s').style.display = this.checked?'block':'none';",	cb.divid), 
		cb.title))
	out(sprintf("<div id='%s'><small>",cb.divid))
	
	otable2(...)
		
	out("</small></div>")
}
