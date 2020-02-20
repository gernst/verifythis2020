# Properties



## Security-Properties

### The server never publishes unverified identities 

**General description:**

The response of a verifying key server never contains an identity, for which the server has not received a verification yet.

**Formal:** 



Let $k$ be some arbitrary identity, $response$ a Boolean predicate defined as
$$
response(k) = 
\begin{cases} true &\mbox{if   } \text{k was included in a response to a client} \\
false &\mbox{if  } otherwise 
\end{cases}
$$
and verified be a predicate defined as 
$$
verified(k) = 
\begin{cases} true &\mbox{if   } \text{k was verified by a client} \\
false &\mbox{if   }otherwise 
\end{cases}
$$
Using these predicates, we define a sequence of actions of a key server to be **valid** if it satisfies the following LTL formula: 
$$
G ( verified(k) \ R \ (\neg response(k)))
$$
For some key $k$: **"It must be true that $k$ is not part of a response until the server receives a verification for $k$. At this point $k$ may be part of a successful key request."**



## Liveness-Properties

Any request of some client $c$ will eventually be met with a response from the key server, regardless of the actual content of the reply. (eg. a key request may return an empty reply, if the given identity hasn't been verified yet.)

Formal: 

Let $r$ be some request and $response_{(r)}$ the corresponding response to $r$ : 
$$
G(c \implies F \ response(c)\ )
$$


