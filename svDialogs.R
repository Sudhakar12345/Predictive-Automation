library(svDialogs)
######## SciViews
library(SciViews)
################################################################
library(gWidgets2)
library(RGtk2)
library(tcltk)
gmessage("Hello world", title="gmessage")
gmessage("Error, Error", title="gmessage",
         icon="error")
a = ginput("Enter your lucky number",text="7",
       title="ginput", icon="question")
gconfirm("Ames is awesome", title="gconfirm")
source(gfile())
setwd(gfile(type="selectdir"))

gbutton("Hello world", cont=TRUE)
gbutton("ok", cont=TRUE)
b5 <- gbutton("Click me for a message", container = TRUE)

gcheckbox("Do you like coke", cont=TRUE)


library(gWidgets2tcltk)
library(gWidgets2RGtk2)
library(tcltk)
library(gWidgets2)

items = c("Coke","Pepsi","None of the above")
gradio(items, cont=TRUE)
gdroplist(items, cont=TRUE)
gdroplist(items, editable=TRUE, cont=TRUE)
gslider(from=0, to = 100, by = 1, cont=TRUE)

msg = "Error...while running the model"
galert(msg, title = "Alert Message", delay = 3, parent = NULL,
       toolkit = guiToolkit())

gbutton(text = "Hello", handler = NULL, action = NULL, container = NULL,
        toolkit = guiToolkit())


#################################################################
#### Select list items
res <- dlgList(month.name, multiple = TRUE)$res
if (!length(res)) {
  cat("You cancelled the choice\n")
} else {
  cat("You selected:\n")
  print(res)
}
##############################
library(svDialogs)

### set the working directory Manually using pop window
setwd(dlgDir(default = getwd())$res)

##### Forms fill manually
dlgForm(form, title = "Fill the form", message = NULL, columns = 1,
        strip.type = TRUE, gui = .GUI)
Form <- list(
  "Name:TXT" = "John Smith",
  "Age:NUM" = 25,
  "Sex:CB" = c("male", "female"),
  "Married:CHK"=FALSE
)
dlgForm(Form, "My form")$res


##### give inputs in pop-up window
a = dlgInput(message = "Enter a value", default = " ", gui = .GUI)$res
a


res <- dlgList(month.name, multiple = TRUE)$res
if (!length(res)) {
  cat("You cancelled the choice\n")
} else {
  cat("You selected:\n")
  print(res)
}
message = "Automatio machine learning model on the way"
dlgMessage(message, type = c("ok", "okcancel", "yesno", "yesnocancel"), gui = .GUI)
