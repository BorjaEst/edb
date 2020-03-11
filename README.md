# edb

<!-- PROJECT SHIELDS -->
<!--
*** I'm using markdown "reference style" links for readability.
*** Reference links are enclosed in brackets [ ] instead of parentheses ( ).
*** See the bottom of this document for the declaration of the reference variables
*** for contributors-url, forks-url, etc. This is an optional, concise syntax you may use.
*** https://www.markdownguide.org/basic-syntax/#reference-style-links
-->
[![Contributors][contributors-shield]][contributors-url]
[![Forks][forks-shield]][forks-url]
[![Stargazers][stars-shield]][stars-url]
[![Issues][issues-shield]][issues-url]
[![License][license-shield]][license-url]


<!-- TABLE OF CONTENTS -->
## Table of Contents

* [About the Project](#about-the-project)
  * [Built With](#built-with)
* [Getting Started](#getting-started)
  * [Prerequisites](#prerequisites)
  * [Installation](#installation)
* [Usage](#usage)
* [Roadmap](#roadmap)
* [Contributing](#contributing)
* [License](#license)
* [Contact](#contact)
* [Acknowledgements](#acknowledgements)



<!-- ABOUT THE PROJECT -->
## About The Project
Set of tools for easily interface mnesia database in erlang.

### Built With
This interface library has been built on top of:
* [mnesia](https://erlang.org/doc/man/mnesia.html)



<!-- GETTING STARTED -->
## Getting Started

### Prerequisites

The easiest way to run this library is through rebar3. You can get it from [rebar3](https://www.rebar3.org/).



### Installation

Create your own project with rebar3.
 ```
 $ rebar3 new app yourapp
 ```

Then in your project path find rebar.config file and add enn as dependency under the deps key:
```
{deps, 
    [
        {edb, {git, "https://github.com/BorjaEst/edb.git", {tag, "<version>"}}}
    ]}.
```

Then using compile command, rebar3 will fetch the defined dependencies and compile them as well for your application.
```
rebar3 compile
```

At the end for making a release you first need to create your release structure and then making a release with following commands.
```
$ rebar3 new release yourrel
$ rebar3 release
```

>You can find more information about dependencies in [rebar3 - dependencies](https://www.rebar3.org/docs/dependencies). 




<!-- USAGE EXAMPLES -->
## Usage

1. Define an id generator with as a tuple of 2 elements, the **Unique_Id** and the **Record_Name**. Use this generator when defining the record as well:
```erlang
-define(ID, {make_ref(), my_record1}).
-record(my_record1, {id = ?ID, data}).
```
>You can do this in the shell by:
```erlang
rd(my_record1, {id={make_ref(), my_record1}, data}).
```

2. Create an attributes table with the fields from the defined records:
```erlang
Attributes_table = [
    {my_record1, record_info(fields, my_record1)},
    ...].
```

3. Create the tables in mnesia using "create_tables":
```erlang
edb:create_tables(Attributes_table),
```

4. Start edb with the desired tables:
```erlang
edb:start(Attributes_table),
```

5. Create and write some inputs. You can do it 1 by 1:
 ```erlang
MyData = #my_record1{data = 0},
edb:write(MyData).
```
>or using a list of inputs:
 ```erlang
MyList = [#my_record1{data = X} || X <- [1,2,3,4]],
edb:write(MyData).
```

6. Recover the data using the id from the record. You can do it 1 by 1:
 ```erlang
MyDataCopy = edb:read(MyData#my_record1.id).
```
>or using a list of ids:
 ```erlang
MyListCopy = edb:read([X#my_record1.id || X <- MyList]).
```

_For more examples, please refer to the [Documentation]()_



<!-- ROADMAP -->
## Roadmap

See the [open issues](https://github.com/BorjaEst/edb/issues) for a list of proposed features (and known issues).



<!-- CONTRIBUTING -->
## Contributing

Contributions are what make the open source community such an amazing place to be learn, inspire, and create. Any contributions you make are **greatly appreciated**.

1. Fork the Project
2. Create your Feature Branch (`git checkout -b feature/AmazingFeature`)
3. Commit your Changes (`git commit -m 'Add some AmazingFeature'`)
4. Push to the Branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request



<!-- LICENSE -->
## License

Distributed under the MIT License. See `LICENSE` for more information.



<!-- CONTACT -->
## Contact

Your Name - [Borja Esteban]()

Project Link: [https://github.com/BorjaEst/edb](https://github.com/BorjaEst/edb)



<!-- ACKNOWLEDGEMENTS -->
## Acknowledgements
* []()



<!-- MARKDOWN LINKS & IMAGES -->
<!-- https://www.markdownguide.org/basic-syntax/#reference-style-links -->
[contributors-shield]: https://img.shields.io/github/contributors/BorjaEst/edb.svg?style=flat-square
[contributors-url]: https://github.com/BorjaEst/edb/graphs/contributors
[forks-shield]: https://img.shields.io/github/forks/BorjaEst/edb.svg?style=flat-square
[forks-url]: https://github.com/BorjaEst/edb/network/members
[stars-shield]: https://img.shields.io/github/stars/BorjaEst/edb.svg?style=flat-square
[stars-url]: https://github.com/BorjaEst/edb/stargazers
[issues-shield]: https://img.shields.io/github/issues/BorjaEst/edb.svg?style=flat-square
[issues-url]: https://github.com/BorjaEst/edb/issues
[license-shield]: https://img.shields.io/github/license/BorjaEst/edb.svg?style=flat-square
[license-url]: https://github.com/BorjaEst/edb/blob/master/LICENSE.txt
[product-screenshot]: images/screenshot.png

