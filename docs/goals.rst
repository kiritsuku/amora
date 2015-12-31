THIS DOCUMENT IS A DRAFT

This document contains a non finished list of points that should be resolved by this research platform. Since there is a lot wrong with the tools we have today, how they work together and how they solve our problems the ideas in this document describe as a whole a new model that, once it is implemented, can move productivity - as I imagine it - a big step forward.

.. contents:: Contents of this document:

Main Goals
==========

There are three main goals of this platform. By solving these three goals, all other ideas and goals that are described throughout this document can be achieved too.

Unified Interface
-----------------

The universal idea is that there should exist only one tool. There shouldn't be a distinction in user area between IDEs, documentation tools, build tools, the command line compiler, the formatting tool, linting tools etc. Of course in developer area these tools should still be different but for users it is a good decision to not care about the differences.

In the following there is an overview of the architecture:

.. code::

    +--------+   +--------------+   +---------+   +--------------+   +-------+
    | Editor |   | Command Line |   | Browser |   | IDE Frontend |   | <...> |             (presentation)
    +---+----+   +------+-------+   +----+----+   +------+-------+   +---+---+
        |               |                |               |               |
        +---------------+----------------+------+--------+---------------+                 (protocol)
                                                |
                                      +---------+--------+
                                      | Tooling Platform |
                                      +---------+--------+
                                                |
         +----------------+-----------------+---+----------------+----------------+        (protocol)
         |                |                 |                    |                |
    +----+-----+   +------+------+   +------+-----+   +----------+--------+   +---+---+
    | Compiler |   | IDE Backend |   | Build Tool |   | Command Line Tool |   | <...> |    (backend)
    +----------+   +-------------+   +------------+   +-------------------+   +-------+

As one can see, all tools in the backend layer are hidden between an unified interface, which is used by different tools in the presentation layer. The presentation layer is meant to be lightweight. It should mostly provide rendering, displaying and user interaction logic. All the data should be stored in the backend layer, were it can easily be shared between multiple frontends.

Component-Oriented Architecture
-------------------------------

The tooling platform that is described so far is heavily decentralized and therefore can even be a distributed computation model. Such an architecture brings its own challenges to the communication model. It is necessary that users of the platform do not have to care about how exactly the communication is happening, therefore we need to abstract over the internals and provide a high level DSL, which allows to address other components of the platform in an easy way.

In the following, such a DSL shall be called Resource Identifiers (RIs). An RI works similar to how an URI works. They are responsible for identifiers components in the system but do not provide any logic on how to resolve them. Instead they rely by themselves on further components that are responsible for resolving all parts of the RI.

TODO define full specification for an RI

Since all RIs delegate the logic to the components of the system, these components shall be called Resource Components (RCs). An RC consists of the following parts:

- Id: A unique string that identifies the RC. It can contain ``a-zA-Z0-9_``
- Parent: A string that identifies another RC. A parent specifies the namespace of all its child RCs.
- Imports: A list of strings that do all specify another RC. Imports are dependencies to other RCs and need to be loaded before the RC with the imports can be used.

TODO define full specification for an RC

TODO mention how queries (e.g. ``find all references of class x``) and commands (e.g. ``compile x``) translate to RCs and RIs

TODO mention how a module system for a language would work together with RIs and RCs

Decentralized Content Hosting Platform for Semantic Information
---------------------------------------------------------------

TODO replace this section by next section, the current one is not really fundamental to the platform

Usually both users and content creators have to use question and answer sites (Stackoverflow, ...), code hosting sites (Github, ...), library hosting sites (Maven Central, ...), personal documentation (Github sites, other hosting platforms, ...), mailing lists, forums, social networks or search engines in order to provide or find content. It is a huge problem that there exist no tool that can access all of these sites at once. Search engines come closest to a central content finding platform but they have the problem that they can't provide semantic information.

Once the points about "Unified Interface" and "Fuse Documentation Tools with IDE Functionality" are realized, it shouldn't be very difficult anymore to receive arbitrary information from anywhere and to store them wherever one likes.

TODO the second mentioned point is not available at this point

Semantic Web
............

TODO this subsection should net be needed

If we look to web technologies, we can see that the web slowly moves away from unstructured data towards structured ones, the so called `web components <http://webcomponents.org/>`_. They basically have the power to allow our tools to understand the web semantically. Without web components, complex heuristics need to be developed if a computer should given the power to understand anything that is going on on a website. With web components, our computers can just match on the name of the web component to know instantly what a web component is supposed to do. Adding the possibility to understand web components to our tools can give us powers we never had had before.

As an example, imagine that StackOverflow is written only with web components. The source code of a question could look like this:

.. code:: html

    <body>
      <question id="2987137">
        <user id="9862364" />
        <content>
          content of the question
        </content>
        <tags>
          <tag name="scala" />
          <tag name="programming" />
        </tags>
      </question>
      <answer id="2356345">
        <user id="1097288" />
        <content>
          some text
          <codeblock lang="scala">
            val x = 0
          </codeblock>
        </content>
      </answer>
      <!-- more answers -->
    </body>

Just by looking at the code we immediately know everything we need to know. A tool can extract the information that the users has a question about the programming language Scala. This means that the tools can enable Scala specific features directly on top of the website. There would be no need for the provider of the HTML code to implement their own syntax highlighting anymore for example - the tool that understands web components can do it. In case the web component are implemented in a library, it would also mean that new websites would take advantage of the provided functionality out of the box - in case they implement their own web components, the tools of course would first have to learn about them.

Notion of Time / Decentralized Nature
-------------------------------------

TODO the spec for RCs and RIs not only needs to handle static but also dynamic content

TODO mention how a decentralized platform needs a notion of time to work properly (i.e reloading of components and components that change over time)

Further Goals
=============

These goals are non essential, i.e. they can be implemented on top of the main goals but are not needed to implement the other goals.

Unite Editor with Browser
-------------------------

Browsers have become powerful platforms - they are finally able to handle the needs of a complex application like an IDE. Not only can they display an editor, they can also render all sort of things - from 2D images to 3D games. Obviously we want to have this power in our IDE. Rendering the commit and branch history of a version control system as a real graph and not just as some ASCII text, rendering the dependency relationships of the classes in our application as a 3D star universe or directly editing the rendered HTML output of our Markdown code are only some of the possibilities modern Browsers can give us. We could literally display everything in any way we like.

Nevertheless, these features don't come for free. If we would implement these features poorly slow performance and huge memory usage would be the result. This is the point where the unified interface to our tooling infrastructure comes to mind. The data would be stored in the backend in an efficient and distinct way. Mapping functions need to be responsible for converting user actions in the UI that operate on rendered data to a internal distinct representation and back again to the rendered output once the internal data changed.

In order that such a mapping can work at all, everything needs to be treated as an editor. Navigating through a text file or editing it should make no difference to navigating or editing a directory explorer or the history view of a VCS. By wiring the mapping functions, which convert from rendered content to internal one and vice versa, we could switch between an arbitrary number of representation on the presentation layer, without changing the representation of the internal data. This is depicted by the following picture:

.. code::

             +------+   +-------------------+   +-------+   +-------+
             | Text |   | Rendered Markdown |   | Graph |   | <...> |
             +--+---+   +---------+---------+   +---+---+   +---+---+
                |                 |                 |           |
                +-----------------+---------+-------+-----------+
                                            |
                                +-----------+-------------+
                                | Internal Data Structure |
                                +-----------+-------------+
                                            |
        +-------------+-------------------+-+----------------+---------------+------------+
        |             |                   |                  |               |            |
    +---+--+   +------+------+   +--------+-------+   +------+------+   +----+----+   +---+---+
    | File |   | Remote File |   | Multiple Files |   | Web Service |   | Process |   | <...> |
    +------+   +-------------+   +----------------+   +-------------+   +---------+   +-------+

As one can see, it doesn't matter how and where a file is stored. There is one single data structure that allows different representations to synchronize their state. It no longer matters if a text editor really represents text, it could also represent rendered HTML code or rendered Markdown code. With this model it is trivially possible to even render different parts of an editor in different ways. A string literal that contains a SQL query can be rendered with SQL highlighting, while the rest of the document are still highlighted by the host language. A doc comment could be converted to HTML and then directly displayed and edited in the editor - users no longer would have to learn and use the data representation that can be understood by the parser of the doc comment.

Fuse Documentation Tools with IDE Functionality
-----------------------------------------------

Nowadays documentation tools are far away from being as good as they could be. This sections mentions a few central aspects which should be the foundations for better tooling support.

Central Interchange Platform for All Sorts of Contributions
...........................................................

This point is a huge milestone and not easy to implement, but absolutely necessary if the subsequent points need to be moved to reality. Instead of forcing every user to host their own website with their documentation, they should all contribute to a single system, which potentially can remove huge contribution barriers.

TODO fix diagram

.. code::

    +---------+   +---------+   +---------+
    | User 11 |   | User 12 |   | User 1N |
    +---+-----+   +---+-----+   +---+-----+
        |             |             |
        +-+-----------+-------------+
          |
    +-----+--+   +----- --+   +--------+   +--------+
    | Orga 1 |   | Orga 2 |   | User 1 |   | Orga N |
    +--------+   +--------+   +--------+   +--------+

    +---------------------------+
    | Central Interchange Point |
    +---------------------------+


Very good and popular examples that show that such a central interchange point is an important feature are StackOverflow and Github. They both revolutionized how communities are organized and how they keep their data. In contrast to these examples, the central interchange point, which I suggest, should support a decentralized model because it need to be available locally, i.e. without Internet access, and may also be hidden behind organization structures, which is both difficult to do if there is only once single central point that keeps all the data. Instead, there should exist some last instance, which at least caches all the public data and some earlier instances, which act mostly as load distribution points but whose data needs to be synchronized with the most central node. This model is similar to how package management for most Linux distributions works, where everyone can contribute packages through third party hosts, but at least there exists a central system that keeps the nodes organized.

Documentation needs to be available directly in the IDE
.......................................................

This point should be straightforward to implement once the IDE is fused with the browser and the interchange platform exists. It would give us the possibility to integrate documentation transparently into IDE functionalities. Modern IDEs provide a lot of useful features like type searching, type hierarchies, code editors, diff viewers and so on, which are all useful features for a documentation tool. Reimplementing them should not be necessary, in fact it is mostly not even done because it would require to much resources to implement all of theses features.

By sharing the IDE implementations with the documentation tools, no further reimplementation of core functionality would be required - instead all resources can be put into user experience regarding displaying data and interacting with it. On the other side, once documentation tools operate on the same data structures as IDEs do, all of the powerful documentation interaction functionality would be available in IDEs. In short, both worlds would profit immensely.

Documentation needs to be interactive
.....................................

StackOverflow showed that static documentation is not enough. Most programming languages generate code documentation out of strings or comments that are embedded in the source code, which is exactly the opposite of interactive or dynamic content. While the documentation can be easily changed when the sources are available, regeneration of the mostly HTML part is still costly. Also, since writing access to the sources is required in order to improve the documentation, most people can't or won't do it. StackOverflow showed how to do it. Everyone who has a StackOverflow account can change the help to improve the content. A built-in reputation system and up- and downvoting functionality form a quality management system, which filters out wrong or just bad content. Since StackOverflow works so well, the default code documentation tool of a programming language should work in a similar way.

But we can go one step further. Documentation does not only mean code documentation, it also means code examples, user and developer documentation, tutorials, books, questions and answers on StackOverflow or on mailing lists, personal homepages and so on and so forth. Everything that relates to a project is documentation. Right now, all existing programming languages have the same problem: They require a huge implicit knowledge base. As a user you have to know a lot of different sources to get documentation and finding these sources often is a process that takes years, in fact this process never finishes because old sources get removed and new sources get added. Today, search engines fill the gap of finding these sources but most search engines have one problem: They do not understand data in a semantic way. You can not tell your search engine what exactly you are looking for. An IDE however has way more semantic knowledge - obviously we want to built tools that support an information search with all of this knowledge. However, getting there is not easy and is explained in the subsequent points.

Built-in quality management
...........................

A platform where a lot of people contribute needs some form of quality management of the content. It can be an automatic one, a manual one or a mix of both. The automatic approach has limitations, since computers do not understand information in a semantic way. With heuristics it would be possible to filter out especially spam content but a manual approach is for sure required for everything that goes beyond that. The principle behind StackOverflow, which relies on up- and downvoting and volunteers that sort out reported content, works very well and therefore should also be supported.

TODO mention how quality management solves the problem about choice (not all people want to have choose which module they should use, the want to use a default one)

User Feedback
.............

TODO Users need to have the chance to easily give feedback about bugs and about their general happiness with the software they use.

Modal Navigating
----------------

Vim showed the strength of modal editing: Simple editing operations can be combined to more complex ones. This is powerful since it avoids the learning of endless of key combinations. Once one finds a way to split a complex operation in more trivial tasks, it is easy to express the problem in Vim.

Modal navigating is the next step forward. Instead of limiting it to text operations (modal editing), it should be possible to control the entire application in a modal way. Modal navigating more or less means that we can switch to different displaying modes, which all can have different functionality associated to them.

As an example, a user could execute git commands of an arbitrary repository and display their results in the text editor. However this is not always the most efficient way to display and even edit the result. In case we display the git history, it would be useful to switch to a git history view, which can display the history as a graphical graph and not only as a textural graph in the editor. This git history view would be a new mode, which not only allows displaying but can also allows editing either through key combinations or through drag & drop of commits, branches or tags with the mouse. In terms of Vim it would mean that the most important Vim modes `Insert`, `Normal` and `Visual Selection` would still be available in the well known way but they would get new siblings in form of new modes.

Given that it is useful to understand different modes as specializations or as abstractions of other modes, modal navigating can be referenced in a more user friendly way as "zooming" with the two operations of "zooming in" and "zooming out" to different levels. On the most outer level, we would be in a "universe" mode, which would give an overview of the current project. It could list the root directory structure, the root package or module structure and other semantic information about the project like the information of the build tool, the name of the project, the contributors or stats. From there we could zoom in to the list of packages or modules, which would enable refactoring support and editing functionality on the text. The next zoom level could be a class or file structure. The classes could be displayed in a UML like way as rectangles, with dependency relationships to each other. From here one could do further zooming to class member level, to expressions level and even to single tokens. As can be seen, each level provides different functionality and displaying/rendering variants.

Switching from one mode to another can easily achieved through key combinations, which is an important features for Vim users. Furthermore, we would also achieve the composition of modes because a mode is uniquely identified by the key combination that leads from one mode to another. Composition also means that switching of modes can be automated and therefore used in scripts.

Scalable and Reactive Platform
------------------------------

In order to understand this point, we first have to understand different user groups:

- The minimalists. These people like to start with something very simple and like to configure the system to match their needs. Furthermore they have no problem to add modules to the system that give them further behavior which is not yet provided by the default installation. This group of people often use editors like Vim, emacs, sublime text etc. because beside from editing they only provide an API that can be used inside of simple scripts. It is not an accident that such editors provide a healthy, rich and easy to use plugin environments.
- The out-of-box users. These people like to use what is known as IDEs. An IDE comes with lots of features and in the best case doesn't require further configuration. It means that one can just install an IDE and start working immediately without customizing anything.
- The customizers that like out-of-box experience. These people are minimalists but nevertheless like the features of IDEs and like to configure their environment in a way that best fits both directions. These people especially like to understand how the entire system fits together but don't necessarily care about specific details. Users of Vim or emacs often fit into this group since they can use a lot of powerful plugins but don't loose control to these plugins.

As we will see, the last group is the one that is mostly ignored by our todays tools and the reason why this document exists. To understand why the first two groups are not enough, we first want to have a look to the group of the minimalists:

- Most editors have the problem that they can't scale. If you add lots of plugins to these environments you easily get into a dependency hell between the plugins. Often, the plugins are written in dynamically typed languages and therefore have scoping problems of variables and configurations. Furthermore, updating these plugins is only easy as long as the plugin itself doesn't depend on other plugins. The moment when you have transitive dependencies you need a powerful plugin manager that can handle all of the dependencies. Such plugin managers are most of the time not part of the editors and therefore scaling them up is extremely difficult.
- Knowing which plugins are of high quality is often not easy. One can rely on how many Github stars a plugin has but if a plugin works together with other plugins is still not easy to find out.
- One needs to understand the design choices of a plugin in order to use it effectively. Getting this knowledge can take a long time. In case a plugin relies on dependencies which are not available in a given system, the plugin can't be used at all. Finding out about these things requires to read the documentation of the project, which not always exist. In a worse case one even has to test the plugin first. This is surely not an optimal solution.

If minimalists have to fight with these problems, why are out-of-the-box users not automatically more happier? This can be explained by these points:

- For IDEs, most of the work about how to put all details together is done by the IDE developers and therefore doesn't need to be done by users.
- However, in practice the out-of-box experience comes with a price. Startup times increase because more plugins need to be loaded and higher memory+cpu consumption can be observed too.
- IDEs generally have the problem that users can't specify which features they want to use in practice. All features are always available and even if they are unused they need to be loaded by the computer.

Because of the above points, the questions arises if it is not possible to unite the best of both worlds but leave out all of the drawbacks. A platform which could achieve this would be interesting to the group of customizers that like out-of-box experience. In fact, such a platform would be interesting to all three groups. With the characteristic that every feature can be enabled or disabled on the fly, the platform would be interesting to minimalists, i.e. users that already use editors like Vim or Sublime Text in their daily lives. By considering from the very beginning that plugins need to work together in order to work correctly in a larger system, the platform would be interesting to IDE users. Plugins that are responsible for a slowdown of the editing experience or even have the property of blocking users entirely - be it because of bugs or because the plugin simply has a lot of work do to - can be easily disabled and therefore allow users to scale down whenever they want (which may be the case on if multiple computers are used for the same configuration, where the computers have different power levels).

First Class Build Tool Integration
----------------------------------

All modern IDEs include their own build tools, which are completely incompatible to all the external (IDE independent) build tools that are out there. This approach leads to the following problems:

- Every IDE needs some metafiles, which include build information for every project.
- Since build tools work differently, their internal information is incompatible to each other. This means that one cannot simply map a build from one build tool to a build of another build tool. It often requires a lot of tweaks and complicated hacks to give an IDE the chance to understand the build of another build tool.
- The metafiles often can be generated automatically but too often they need to be created or edited manually by the user in a cumbersome way. As discussed in the previous point, this is because the build tools are incompatible to each other.

These problems in practice are the reason why a lot of people ignore IDEs completely because they don't want to invest time to "fix the IDE". People already have projects and the IDE should be able to understand these projects immediately without another configuration step. With the help of RIs and RCs such an immediate understanding should be easy to implement. We only need RCs for the most important build steps (compiling, running, debugging etc.) and we are done. The IDE simply forwards all the work to the external build tool and communicates with it through RCs. Since all other components of the IDE already understand RCs, they simply can understand all of the information of the build tool. Incompatibilities are gone and users can concentrate on fixing their programs instead of fixing the IDE.

Influence Software Development Culture to Value OSS More
--------------------------------------------------------

TODO
