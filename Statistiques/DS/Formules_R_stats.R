--------------------------------------------------------------------------------------------------------------------------------------------
"Statistiques descriptives"

    "Graphes"

        #Raccourci ligne/colonne
            variable <- data[ligne,colonne] #ex: data[1,1:11] (ligne 1, colonne 1 à 11)
    
        #Histogramme :
            hist(x=data, break=seq(min,max,ecart), main='Titre', xlab="Nom axe abscisses", ylab="Nom axe ordonnées")
                      #break -> permet de séparer en classe l'axe des abscisses
    
        #Diagramme des fréquences cumulés
            plot(classe, data, type='s') 
        
        #Diagramme batôn
            barplot(donnée_abscisse, density=donnée_ordonnée, main="titre")
            
        #Diagramme circulaire
            pie(tabdata_effectif, label=tabdata_val)
      
        
    "Calculs"

        #Médiane 
            mediane = quantile(data, prob=0.5)
        
        #Moyenne
            moyenne = sum(tabdata_val*tabdata_occurence)/taille_echantillon   #occurence-> nbr de fois val
        
        #Ecart-type
            ecart_type = sqrt(sum(tabdata_occurence*(tabdata_val-moyenne)^2/(taille-1)))
        
        #Intervalle interquatile
            Q1 = quantile(data,prob=0.25)
            Q3 = quantile(data, prob=0.75)
            interquartile = Q3 - Q1
            
        #Intervalle interdécile
            interdecile = quantile(data, prob=0.9) - quantile(data,prob=0.1)
    
        #Mode 
            "valeurs les plus fréquentes"
   
         
--------------------------------------------------------------------------------------------------------------------------------------------  
"Estimation"       
    
    "Calcul"
    
        #Moyenne empirique
            mu_e = mean(data)
    
        #Variance empirique
            variance_e = var(data)
    
        #Ecart-type empirique corrigé
            sigma_e_c = sd(data)
        
        #Ecart-type empirique non corrigé 
            sigma_e = sigma_e_c*sqrt((n-1)/n)
        
    
    "Estimation de l'esperance ou moyenne"
    
        #Si loi normale
            
            #Variance connu :
                interval_min = mue - qnorm(1-alpha/2)*sigma/sqrt(taille)  #ex: seuil de 5% -> alpha=0,05
                interval_max = mue + qnorm(1-alpha/2)*sigma/sqrt(taille)
    
            #Variance inconnu :
                interval_min = mu_e - qt(1-alpha/2,taille-1)*sigma_e/sqrt(taille-1)
                interval_max = mu_e + qt(1-alpha/2,taille-1)*sigma_e/sqrt(taille-1)
                #ou avec sigma emp corrigé
                interval_min = mu_e - qt(1-alpha/2,taille)*sigma_e_c/sqrt(taille)
                interval_max = mu_e + qt(1-alpha/2,taille)*sigma_e_c/sqrt(taille)
      
        #Si pas loi normale
                
            #Variance connu
                interval_min = mue - qnorm(1-alpha/2)*sigma/sqrt(taille)
                interval_max = mue + qnorm(1-alpha/2)*sigma/sqrt(taille)
            
            #Variance inconnu
                interval_min = mue - qnorm(1-(alpha/2))*sigma_e/sqrt(taille-1)
                interval_max = mue + qnorm(1-(alpha/2))*sigma_e/sqrt(taille-1)
                #ou avec sigma emp corrigé
                interval_min = mue - qnorm(1-(alpha/2))*sigma_e/sqrt(taille)
                interval_max = mue + qnorm(1-(alpha/2))*sigma_e/sqrt(taille)
        
    
    "Estimation de la variance ou écarts-type"
    
        #Si espérance connue
            interval_min =
            interval_max = 

        #Si espérance inconne
            interval_var_min = (taille*sigma_e**2)/qchisq(1-alpha/2,taille-1)
            interval_var_max = (taille*sigma_e**2)/qchisq(alpha/2,taille-1)
            #ou avec variance corrigé
            interval_var_min = ((taille-1)*sigma_e_c**2)/qchisq(1-alpha/2,taille-1)
            interval_var_max = ((taille-1)*sigma_e_c**2)/qchisq(alpha/2,taille-1)
    
    
    "Estimation de la proportion"
        
        #Wilson
            #avec correction de continuité -> utilise loi normale
                prop.test(nombre_ds_echantillon, taille_echantillon, p=proba_reelle)
            
            #sans correction de continuité -> utilise loi binomiale
                binom.test(nombre_ds_echantillon, taille_echantillon, p=proba_reelle)
    
        #Wald
            p_emp = nombre_ds_echantillon/taille_echantillon
            intervalmin = p_emp - qnorm(1-alpha/2)*sqrt(p_emp*(1-p_emp)/taille)
            intervalmax = p_emp + qnorm(1-alpha/2)*sqrt(p_emp*(1-p_emp)/taille)
    
--------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------
"Tests : comparaison à une valeur théorique"

    "Espérance/moyenne"
        
        #Si variance connu
            score = sqrt(taille)*(mu_e-mu_0)/sigma    #mu_0 -> énoncé
            q = qnorm(1-alpha/2)    #quantile -> loi normale
        
        #Si variance inconnue
            score = sqrt(taille)*(mu_e-mu_0)/sigma_e_c
            q = qt(1-alpha,taille-1)    #quantile -> loi student
            #ou
            t.test(data, alternative="",mu=mu_0)
                  #alternative -> greater : unilateral sup, less : unilateral inf, two.sided : bilaterale
         
               
    "Variance"
      
            chisq.test(data)  
           
             
    "Porportion"
      
        score = (p_emp-p0)/(sqrt((p0*(1-p0))/taille))
        q = qnorm(1-alpha/2)    #quantile de loi normale
        #ou
        prop.test(nbr_reel_ech, taille_ech, p=p0, correct=F)

--------------------------------------------------------------         
"Tests : comparaison de deux échantillons indépendants"

    "Esperance/moyenne"
      
        #Si les variances sont connues
            score = (mu_e_a-mu_e_b)/sqrt((sigma_a**2)/taille_a + (sigma_b**2)/taille_b)
            q = qnorm(1-alpha/2)     #quantile de loi normale
        
        #Si les variances sont inconnues
            t.test(data, alternative="",mu=mu_0)
    
            
    "Variance"
        
        score = (sigma_e_c_a**2)/(sigma_e_c_b**2)
        q1 = qf(0.025,taille_a-1,taille_b-1)
        q2 = qf(0.975,taille_a-1,taille_b-1)
        #ou 
        var.test(data_a,data_b)
    
        
    "Proportion"
    
        rho = (taille_a*p_a_e + taille_b*p_b_e)/(taille_a+taille_b)
        score = (p_a_e-p_b_e)/sqrt(rho*(1-rho)*(1/taille_a+1/taille_b))
        q = qnorm(1-alpha/2)  #si bilaterale
        q = qnorm(1-alpha)  #si unilaterale
        #ou
        prop.test(c(nbr_reel_echa,nbr_reel_echb), c(taille_echa,taille_echb), alternative="", correct=F)
            
            
            
            
                   