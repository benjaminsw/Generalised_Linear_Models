# Admissions to medical school

# Load the package containing the data
library(Stat2Data)
data(MedGPA)

# Plot the data allowing for some jitter to better see overlapping points:
library(ggplot2)

medgpa.plot <- ggplot(data = MedGPA, aes(y = Acceptance, x = GPA)) +
  geom_jitter(width =0, height =0.01, alpha =0.5, colour ="#984ea3")

medgpa.plot <- medgpa.plot +
  theme(panel.background = element_rect( fill = "transparent", colour = NA),
        plot.background = element_rect( fill = "transparent", colour = NA),
        panel.border = element_rect(fill = NA, colour = "black", size = 1))

medgpa.plot + geom_smooth(method = "lm", se = FALSE,
                          fullrange = TRUE, colour = "#984ea3")

# Linear model:
med.lm <- lm(Acceptance ~ GPA, data=MedGPA)
summary(med.lm)

# Logistic regression (logit) model:

med.glm <- glm(Acceptance ~ GPA, data = MedGPA, family = binomial)
summary(med.glm)

# Overlay the fitted logistic regression curve onto the previous plot:
medgpa.plot +
  geom_smooth(method = "lm",se = FALSE, fullrange = TRUE, colour = "#984ea3") +
  geom_smooth(method = "glm", color = "#ff7f00",se = FALSE, fullrange = TRUE,
              method.args = list(family = "binomial"))