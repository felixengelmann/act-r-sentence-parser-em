# Model 5
# Naming convention: model-latencyfactor-noise
model5.4615 <- c(420, 644,688,636,424,421)
model5.4630 <- c(463, 620, 699, 630, 469, 430)
model5.4645 <- c(482, 602,679, 580,491,441)

model5.1415 <- c(181, 235, 250, 234, 182, 181)
model5.1430 <- c(185, 232, 250, 228, 152,182)
model5.1445 <- c(192, 224,260,228,197,187)

allmodel5 <- rbind(model5.4615,model5.4630,model5.4645,model5.1415,model5.1430,model5.1445)
model5.46 <- rbind(model5.4615,model5.4630,model5.4645)

# Model 6
model6.2615 <- c(548, 728, 858, 731, 554, 539)
model6.2630 <- c(540, 616, 654, 666, 537, 522)
model6.2645 <- c(510, 582, 746, 516, 553, 405)

model6.26 <- rbind(model6.2615,model6.2630,model6.2645)

# Exp. Data
load("polaritydata.Rda")

## exercise 4:
head(d.rs)

## remove values less than 50ms
d.rs <- subset(d.rs,value>50)

## compute means and 95% CIs
means.sfd <- cast(d.rs, condition ~ ., function(x) c(M=round(mean(x)), ci=round(ci(x)) ), subset=times=="SFD")
means.ffd <- cast(d.rs, condition ~ ., function(x) c(M=round(mean(x)), ci=round(ci(x)) ), subset=times=="fpFFD")
means.fprt <- cast(d.rs, condition ~ ., function(x) c(M=round(mean(x)), ci=round(ci(x)) ), subset=times=="FPRT")
means.rbrt <- cast(d.rs, condition ~ ., function(x) c(M=round(mean(x)), ci=round(ci(x)) ), subset=times=="RBRT")
means.rrt <- cast(d.rs, condition ~ ., function(x) c(M=round(mean(x)), ci=round(ci(x)) ), subset=times=="RRT")

means.rrtold <- cast(subset(d.rs,value>50), condition ~ ., function(x) c(M=round(mean(x)), ci=round(ci(x)) ), subset=times=="RRT")

means.rpd <- cast(d.rs, condition ~ ., function(x) c(M=round(mean(x)), ci=round(ci(x)) ), subset=times=="RPD")
means.trt <- cast(d.rs, condition ~ ., function(x) c(M=round(mean(x)), ci=round(ci(x)) ), subset=times=="TFT")


