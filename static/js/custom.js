$(document).ready(function() {
    $("li").click(function(){
        "use strict";
        var content = $(this).text();
        if (content === "Register") {
            $("#remember-label").remove();
            if ($("#confirm-div").length === 0){
                $("#input-div").append(
                    "<div class=\"form-group\" id=\"confirm-div\">"
                      + "<div class=\"input-group\">"
                        + "<p class=\"input-group-addon\">"
                          + "<i class=\"fa fa-fw fa-lock\"></i>"
                        + "</p>"
                        + "<input class=\"form-control\""
                              + " id=\"cpassword\""
                              + " name=\"cpassword\""
                              + " type=\"password\""
                              + " placeholder=\"retype password\">"
                      + "</div>"
                    + "</div>");
            }
        }
        if (content === "Login") {
            $("#confirm-div").remove();
            if ($("#remember-label").length === 0) {
                $("#remember-div").prepend("<label id=\"remember-label\"><input id=\"remember\" type=\"checkbox\">Remember Me</label>");
            }
        }
        $("#login-btn").text(content);
        $("#login").attr("action", "/" + content.toLowerCase());
        $("#login-h2").text(content);
    });
});
