#Comments on lines 16 and 26
rotor1a <- c(26, 25, 12, 1, 20, 7, 5, 13, 23, 18, 24, 17, 15, 9, 14, 11, 4, 19, 6, 16, 10, 22, 2, 3, 21, 8);
rotor1b <- c(13, 19, 23, 2, 7, 18, 3, 12, 11, 4, 9, 5, 6, 26, 20, 14, 10, 25, 8, 16, 1, 17, 21, 22, 15, 24);

rotor2a <- c(23, 9, 24, 5, 16, 14, 20, 13, 6, 12, 21, 22, 1, 25, 18, 2, 8, 4, 10, 15, 17, 3, 7, 11, 26, 19);
rotor2b <- c(24, 4, 5, 3, 20, 25, 9, 12, 11, 6, 2, 21, 7, 8, 1, 19, 13, 10, 17, 26, 14, 15, 22, 16, 18, 23);

rotor3a <- c(24, 12, 17, 9, 16, 4, 22, 21, 8, 14, 18, 2, 3, 19, 25, 7, 11, 15, 10, 6, 5, 26, 20, 23, 13, 1);
rotor3b <- c(11, 23, 9, 24, 12, 3, 17, 21, 20, 10, 15, 4, 19, 5, 18, 8, 13, 26, 22, 2, 6, 14, 16, 7, 1, 25);

reflector <- c(17, 20, 24, 21, 23, 26, 22, 15, 16, 18, 14, 19, 25);

letters <- c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z");

take_string <- function() {
  #Change the messages in the following line of code (encrypted message goes here as well, if you want to translate back):
  initial_str <- "THISISATESTOFMYENIGMAMACHINEPROGRAM"
  list_str <- strsplit(initial_str, "");
  to_change <- unlist(list_str);
  encrypt (to_change);
}

encrypt <- function(to_encrypt) {
  changed <- c();
  
  #Change the positions, each ranging from 0-25 here:
  pos1 <- 9;
  pos2 <- 8;
  pos3 <- 11;
  
  for (letter in to_encrypt) {
    
    let_ind <- match(letter, letters);
    
    start <- let_ind - pos1;
    if (start < 1) {
      start <- start + 26;
    }
    
    num1a <- rotor1a[start];
    pos1b <- match(num1a, rotor1b);
    pos_n1 <- pos1b + pos1;
    if (pos_n1 > 26) {
      pos_n1 <- pos_n1 - 26;
    }
    
    second <- pos_n1 - pos2;
    if (second < 1) {
      second <- second + 26;
    }
    
    num2a <- rotor2a[second];
    pos2b <- match(num2a, rotor2b);
    pos_n2 <- pos2b + pos2;
    if (pos_n2 > 26) {
      pos_n2 <- pos_n2 - 26;
    }
    
    third <- pos_n2 - pos3;
    if (third < 1) {
      third <- third + 26;
    }
    
    num3a <- rotor3a[third];
    pos3b <- match(num3a, rotor3b);
    pos_n3 <- pos3b + pos3;
    if (pos_n3 > 26) {
      pos_n3 <- pos_n3 - 26;
    }
    
    if (pos_n3 < 14) {
      re_num <- reflector[pos_n3];
    } else {
      re_num <- match(pos_n3, reflector);
    }
    
    re_start <- re_num - pos3;
    if (re_start < 1) {
      re_start <- re_start + 26;
    }
    
    re_num3b <- rotor3b[re_start];
    re_pos3a <- match(re_num3b, rotor3a);
    re_pos_n3 <- re_pos3a + pos3;
    if (re_pos_n3 > 26) {
      re_pos_n3<- re_pos_n3 - 26;
    }
    
    re_second <- re_pos_n3 - pos2;
    if (re_second < 1) {
      re_second <- re_second + 26;
    }
    
    re_num2b <- rotor2b[re_second];
    re_pos2a <- match(re_num2b, rotor2a);
    re_pos_n2 <- re_pos2a + pos2;
    if (re_pos_n2 > 26) {
      re_pos_n2 <- re_pos_n2 - 26;
    }
    
    re_third <- re_pos_n2 - pos1;
    if (re_third < 1) {
      re_third <- re_third + 26;
    }
    
    re_num1b <- rotor1b[re_third];
    re_pos1a <- match(re_num1b, rotor1a);
    re_pos_n1 <- re_pos1a + pos1;
    if (re_pos_n1 > 26) {
      re_pos_n1 <- re_pos_n1 - 26;
    }
    
    new_letter <- letters[re_pos_n1];
    changed <- c(changed, new_letter);
    
    pos1 <- pos1+1;
    if (pos1 == 26) {
      pos1 <- 0;
      
      pos2 <- pos2+1;
      if (pos2 == 26) {
        pos2 <- 0;
        
        pos3 <- pos3+1;
        if (pos3 == 26) {
          pos3 <- 0;
        }
      }
    }
  }
  
  print(paste(changed, collapse=""));
  
}
take_string();