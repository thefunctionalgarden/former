former
=====

Very easy way to call your Erlang functions from a browser.

How to use
-----
- add the dependency to your project rebar.config
```erlang
      {former, {git, "git@github.com:....git", {branch, "master"}}}
```

- declare the functions you want to call in a map, like this:
```erlang
TestForm1 = #{
    submit => #{
        module => you_module_name, 
        function => your_function_name
    }
},
```

- start former, passing a list of forms, and the HTTP port you want:
```erlang
    former:start([TestForm1], 10100),
```

- open you browser: http://localhost:10100/former
