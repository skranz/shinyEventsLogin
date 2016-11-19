flexTextInput = function (inputId, label="", value = "", class="form-control", width = NULL, placeholder = NULL, autocomplete="off",...)
{
    div(class = "form-group shiny-input-container", style = if (!is.null(width))paste0("width: ", validateCssUnit(width), ";"),
      tags$label(label, `for` = inputId),
      tags$input(id = inputId,type = "text", class = class, value = value,placeholder = placeholder,autocomplete=autocomplete,...)
    )
}