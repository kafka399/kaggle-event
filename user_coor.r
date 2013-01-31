user_no_coor=user[which(is.na(user$user_lat)),]
which(as.character(user$user_id)%in%strsplit(as.character(friends[which(friends$user==user_no_coor$user_id[1]),2]),' ')[[1]])

