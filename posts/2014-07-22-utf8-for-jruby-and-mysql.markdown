---
layout: post
postTitle:  UTF-8 for JRuby and MySQL
categories: blog
---

As we all know, string encoding is fun and pleasant.

If your MySQL database uses the UTF-8 charset, you'll probably want JRuby to use
UTF-8. To start JRuby (or anything else on the JVM) with UTF-8 as the default
encoding:


```bash
export JAVA_TOOL_OPTIONS='-Dfile.encoding=UTF-8'
```

We can test this, in `jirb`:

```ruby
>> "".encoding
=> #<Encoding:UTF-8>
```

Now we can configure JDBC to connect with the proper encoding:

```ruby
sql_server = 'jdbc:mysql://my_host/my_database?useUnicode=yes&characterEncoding=UTF-8'
DB = Sequel.connect(sql_server, ...)
```

