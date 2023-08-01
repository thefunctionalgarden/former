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


Limitations
-----
- Function params are passed as bitstrings.
- Layout of the form is currently very basic.

