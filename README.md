former
=====

Very simple way to call your Erlang functions from a web browser, passing params through a form.

How to use
-----
- add the dependency to your project rebar.config
```erlang
{former, {git, "git@github.com:thefunctionalgarden/former.git", {branch, "main"}}}
```

- somewhere in you code declare the function you want to call, like this:
```erlang
    TestForm1 = #{
        submit => #{
            module => your_module_name, 
            function => your_function_name
        }
    },
```

- start **former**, passing a list of forms, and the HTTP port of your choice:
```erlang
    former:start([TestForm1], 10100),
```

- open you browser: http://localhost:10100/former

![image](https://github.com/thefunctionalgarden/former/assets/82588439/5bdc7111-153d-4b5c-8910-c772e156f6b8)



Limitations
-----
- the layout of the form is currently very basic.
- function params are passed as bitstrings.
- function return is processed by jsx:encode(), be nice with it.
- if you have 2 or more functions, the order of the functions in the form is a surprise.

