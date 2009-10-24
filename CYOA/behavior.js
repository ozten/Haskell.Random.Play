var submitForm = false;
$(document).ready(function(){
    
    $('input[name=page_name]').keyup(function(evt){
            console.info("keyup");
            var v = $('input[name=page_name]').attr("value");                
            v = v.replace(/[^.a-zA-Z0-9_-]/, '');            
            $('input[name=page_name]').attr("value", v);
            return true;
        })
        .focus();
    $('input[name=page_name]').keypress(function(evt){
        console.info("keypress Seeing ", evt.keyCode);
            if (evt.keyCode == 13) {
                submitForm = false;
                return false;
            }
    });
    $('#Save-btn, #Cancel-btn').click(function(){
        console.log("handeling click SubmitFor is ", submitForm);
        submitForm = true;
    });
    
    $('#create-page-form').submit(function(evt, f){
        window.evt = evt;
        window.f = f;
        console.log("handeling submit SubmitForm is ", submitForm);
        return submitForm;
    });
});