---
output:
  html_document: default
  pdf_document: default
  word_document: default
---
Загрузите данные в датафрейм. Адрес: github    https://raw???путь_к_файлу_найдите_сами???/data/gmp.dat 
```{r}
gmp <- read.table(file="gmp.dat")

gmp$pop <- gmp$gmp / gmp$pcgmp
estimate.scaling.exponent <- function(a, y0=6611, response=gmp$pcgmp,
                                        predictor = gmp$pop, maximum.iterations=100, deriv.step = 1/100,
                                        step.scale = 1e-12, stopping.deriv = 1/100) {
  
  # проверка корректности входных данных
  stopifnot(is.numeric(a),length(a)==1)
  stopifnot(is.numeric(y0),length(y0)==1)
  stopifnot(is.numeric(deriv.step))
  
  mse <- function(a) { mean((response - y0*predictor^a)^2) }
  for (iteration in 1:maximum.iterations) {
    deriv <- (mse(a+deriv.step) - mse(a))/deriv.step
    a <- a - step.scale*deriv
    if (abs(deriv) <= stopping.deriv) { break() }
  }
  fit <- list(a=a,iterations=iteration,
              converged=(iteration < maximum.iterations))
  return(fit)
}
```

Пример вызова с начальным значением a=0.15 :
```{r}
res.1 <- estimate.scaling.exponent(0.15)
res.1
```

С помошью полученного коэффициента постройте кривую (функция curve) зависимости.

```{r}
y0 <- 6611 # параметр распределения

plot (gmp$pop, gmp$pcgmp, xlab = "Население", ylab = "Доход на душу населения", log="xy")
curve(y0*(x)^(res.1$a), add=TRUE, col="red") # построение модели с подобранным коэффициентом
```

Удалите точку из набора исходных данных случайным образом, как изменилось статистическая оценка коэффициента a?

```{r}
random.number <- round(runif(1, min=1, max=nrow(gmp))) # число из нормального распределения на интервале от 1 до кол-ва строк датафрейма gmp
random.number
nrow(gmp) # количество строк (точек) до удаления
gmp <- gmp [-random.number,]
nrow(gmp) # количество строк после удаления
```

Вызовем функцию с начальным значением a=0.15 еще раз:
```{r}
res.2 <- estimate.scaling.exponent(0.15)
res.2$a # полученный коэффициент
res.2$a-res.1$a # разница значений полученных коэффициентов
```

При удалении случайной строки из датафрейма статистическая оценка коэффициента _а_ может как увеличиться, так и уменьшиться.

#Запустите оценку несколько раз с разных стартовых точек. Как изменилось значение a?
```{r}
estimate.scaling.exponent(a=0.15, y0=3000)$a
estimate.scaling.exponent(a=0.15, y0=4000)$a
estimate.scaling.exponent(a=0.15, y0=5000)$a
estimate.scaling.exponent(a=0.15, y0=6000)$a
estimate.scaling.exponent(a=0.15, y0=7000)$a
```

При увеличении значения стартовой точки значение коэффициента _a_ уменьшается.
