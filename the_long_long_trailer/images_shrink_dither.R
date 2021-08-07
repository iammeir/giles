setwd("/Users/gilesgraham/Dropbox/website/iammeir.github.io/the_long_long_trailer")
library(magick)
library(tidyverse)
library(readtext)

# list original .md docs
original_emails <- list.files() 

# interpret them
emails <- map(original_emails[str_detect(original_emails,".*\\.md")],readtext)

# pull raw images
images <- map(dir("imgs"),~image_read(paste("imgs/",.x,sep="")))

# reduction function
shrink_n_dither <- function(x){
  # shrinks them,
  image_resize(x,geometry_size_pixels(width = 640)) %>% 
    # then dithers
  image_quantize(max = 12, colorspace = "YPbPr") 
  # returns implicitly
}

new_images <- map(images,shrink_n_dither)

new_image_names <- paste("img/",str_replace(dir("imgs"),"\\..*",""),".jpeg",sep="")

# writes the files out
# doesn't need assigning
# but good practice.
new_image_catch <- map2(
  new_images,
  new_image_names,
  ~image_write(
    .x,
    .y,
    format="jpeg")
  )

# replacing links in newsletters ----

image_ref <- map(emails,~str_extract_all(.x$text[1],"!\\[.*\\]\\(.*\\)"))

# this little function assigns a consistent inner index to the scraps of image urls
inner_index <- function(title,index){
  tibble(a = unlist(title)) %>% 
    mutate(b = row_number(),
           intind = paste(index,"_",b,sep="")) %>% pull(intind)
}

# stuff it into a tibble for convenience
images_swapper <- tibble(old_title = image_ref) 

# what a terrible function name

# this function takes a text, and a list of pairs,
# then iterates over each pair to swap them in the text.
# it returns the changed text

feed <- function(text,pairs){
  newtext <- text
  if(nrow(pairs)>0){
    for(i in 1:nrow(pairs)){
      
      newtext <- str_replace(newtext,fixed(pairs$pattern[i]),pairs$replace[i])
    }
  }
  newtext
}

# alright here's the big one. hold on
new_emails <- tibble(old_emails = emails) %>%
  # take the email texts.
  # index them from the filenames.
  mutate( index = map(emails,~str_extract(.x$doc_id,"[0123456789]{1,2}")),
          index = as.numeric(index)) %>%
  # here I rely on the fact that nothing has changed the order of these lists.
  # it's not best practice but it works fine here, where I am in fact doing no ordering of things.
  bind_cols(images_swapper) %>% 
  mutate(
    # I create inner indexes for each image
         img_index = map2(old_title,index,inner_index),
    # I need both urls for finding and replacing in the text
         old_url = map(old_title,~str_extract(unlist(.x),"\\(.*\\)")),
    # I have done this before, but not nested like this, so it bears repeating
         new_url = map(img_index,~paste("(img/",.x,".jpeg)",sep=""))
  ) %>% 
  # now I set up for the feed function, and a few webpage formalities.
  mutate(swap_pairs = map2(old_url,new_url,~tibble(pattern = .x,replace = .y)),
         # the gold
         outputemail = map2_chr(old_emails,swap_pairs,~feed(.x$text,.y)),
         # maybe I should just write over the original? nah.
         # tllt = the long long trailer
         output_name = map_chr(old_emails,~paste("tllt_",.x$doc_id,".md",sep="")),
         output_title = map_chr(old_emails,~paste(.x$doc_id,sep="")))

save.image("back_up_fixed_version.Rdata")

# now I need to save the new emails as markdown.

# load my email template
source("email_template_strings.R")

final_emails <- new_emails %>% 
  mutate(
    output_chunk = outputemail,
    output_page = paste(
      email_top_1,
      output_name,
      email_top_2,
      output_name,
      email_top_3,
      output_name,".html",
      email_top_4,
      # this seems unneccesarily fiddly, but I'm not changing it.
      "<h1>",str_replace(str_replace(output_title,"\\.md",""),"_"," "),"</h1>",
      output_chunk,
      email_bottom,
      sep=""
    )
  )

# now I save them as .md and roll pandoc over it.

catch <- map2(final_emails$output_page, final_emails$output_name, ~write_file(.x,.y))

clipr::write_clip(knitr::pandoc(final_emails$output_name,format = "html"))

