var $ = jQuery;

var footer = '<div class="footer"><div class="container"><p>Project Jetson is a predictive analytics project aimed at providing predictions on displaced population movement for the Somalia situation launched by UNHCR’s Innovation Service. The purpose of this website is to enhance information-sharing of data innovation processes. The information shown on this site should be viewed as indicators of potential movements and their underlying causes. UNHCR does not represent or endorse the accuracy or reliability of any advice, opinion, or analysis developed as a result of the use of predictions data provided on this website. Reliance upon any such advice, opinion or analysis is at your own risk. If you would like to know more about Project Jetson, please reach out to UNHCR Innovation Service, at <a href="mailto:innovation@unhcr.org" target="_blank">innovation@unhcr.org</a>.<br> We use cookies and other identifiers to help improve your online experience. By using our website you are agreeing to this. Read our <a href="http://www.unhcr.org/privacy-policy.html">privacy policy</a> to find out what cookies are used for and how to change your settings.</p></div></div>';

$(document).ready(function(){
    setTimeout(function () {
                $('.container').append(footer);
}, 500);
    
})
