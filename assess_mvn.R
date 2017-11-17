function (data) {

    m2_table <- c('Group','M_2','plevel (two-tail)')

    for (jj in 1:length(data)) {

        group <- data[[jj]]

        S <- cov(group, use='pairwise')
        col_means <- colMeans(group, na.rm=TRUE)
        D2 <- mahalanobis(group,col_means,S)
        a2p <- sum(D2^2, na.rm=TRUE)

        p <- ncol(group)
        n <- nrow(group)
        E <- n - p*(p + 2)
        sd <- sqrt(8*p*(p + 2))
        M2 <- (a2p/E)/sd

        if (M2 > 0) {
            p_m2 <- 2*(1-pnorm(M2))
        } else {
            p_m2 <- 2*pnorm(M2)
        }
        M2 <- round(M2, 3)
        p_m2 <- round(p_m2, 3)

        new_row <- c(jj, M2, p_m2)
        m2_table <- rbind(m2_table, new_row)
    }

	row.names(m2_table) <- NULL

    return(m2_table)
}
