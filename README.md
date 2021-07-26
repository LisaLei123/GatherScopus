# GatherScopus

GatherScopus package is designed to make it easy and quick to get the data from Scopus database. 

## Installation

```{r}
# Install release version from CRAN
install.packages("GatherScopus")

# Install development version from GitHub
devtools::install_github("LisaLei123/GatherScopus")
```

## Steps to get API key

See https://github.com/muschellij2/rscopus 

In order to use this package, you need an API key from https://dev.elsevier.com/sc_apis.html. You should login from your institution and go to Create API Key. You need to provide a website URL and a label, but the website can be your personal website, and agree to the terms of service.

1. Go to https://dev.elsevier.com/user/login. Login or create a free account.

2. Click ???Create API Key???. Put in a label, such as rscopus key. Add a website. http://example.com is fine if you do not have a site.

3. Read and agree to the TOS if you do indeed agree.

4. Add Elsevier_API = "API KEY GOES HERE" to ~/.Renviron file, or add export Elsevier_API=API KEY GOES HERE to your ~/.bash_profile.

Alternatively, you you can either set the API key using rscopus::set_api_key or by options("elsevier_api_key" = api_key). You can access the API key using rscopus::get_api_key.

You should be able to test out the API key using the [interactive Scopus APIs](https://dev.elsevier.com/scopus.html).

#### A note about API keys and IP addresses

The API Key is bound to a set of IP addresses, usually bound to your institution. Therefore, if you are using this for a Shiny application, you must host the Shiny application from your institution servers in some way. Also, you cannot access the Scopus API with this key if you are offsite and must VPN into the server or use a computing cluster with an institution IP.

See https://dev.elsevier.com/tecdoc_api_authentication.html

#### Get a Token

As per https://dev.elsevier.com/tecdoc_api_authentication.html: ???Using a proprietary token (an???Institutional Token???) created for you by our integration support team???, so you need to contact Scopus to get one. 

## Example

This is a simple example of how to use the package to get the data you want from Scopus database.

```{r}
library(GatherScopus)

api_key = '[Your API Key]'
insttoken = '[Your Token]'

                      c('journal name',                                                                'issn') # https://portal.issn.org/
# ---------------------------------------------------------------------------------------------------|------------- 
journal_info <- rbind(c('International Journal of Computer Supported Collaborative Learning',         '1556-1607'), 
                      c('Journal of the Learning Sciences',                                           '1050-8406'))

journals = journal_info[,2]

gather_scopus(journals, api_key, insttoken)

```








