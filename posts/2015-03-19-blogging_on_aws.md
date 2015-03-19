---
title: Blogging on AWS
tags: site
---

This site's been through a number of iterations. From a [self-hosted][php] PHP
application running on a box under the sofa, to a [dynamic][yesod] Yesod
application deployed to a Digital Ocean VPS, to a [static][] site built with
Jekyll, now Hakyll, but still deployed to that VPS. The most recent iteration
has been to move that site (and the various supporting subdomains) entirely to
Amazon S3.

[php]: /posts/php_authentication/
[yesod]: /posts/site_migration/
[static]: /posts/on_staticness/

This allowed me to ditch the Digital Ocean box, replace `rsync` with `s3cmd`,
and continue serving my blog with a reduced cost and operations overhead. A few
days after this switch, a coworker pointed me to a TLS error when visiting my
site over HTTPS. "Why would you do that?" I asked, "I'm not serving over HTTPS."
His reply was insightful: "when I check if a website is available via HTTPS, I
should either find a properly configured site or Connection Refused". Since I
had moved to S3, my domain was now listening on port 443, but since I'd not
configured SSL, it wasn't working. Now I had a problem.

## Original Source

If you Google some incantation involving "S3", "blog", and "SSL", you'll
invariably arrive at this result: [Setting Up SSL on AWS CloudFront and
S3][bryce]. It's a great post, and this one will share much of its content.
Unfortunately, there was still some information not present in it that I had to
guess at or hunt down myself. Presenting that "original content" requires that I
restate much of what's there for context. If you're attempting such a setup, I
recommend reading both posts as part of your due diligence.

[bryce]: https://bryce.fisher-fleig.org/blog/setting-up-ssl-on-aws-cloudfront-and-s3/

### Tools

For the initial SSL setup, I use [aws-cli][]. For the day-to-day of updating and
creating content in the S3 bucket (i.e. writing blog posts), I use [s3cmd][].
The reason for the two tools is that `aws` is required for many service
administration tasks, which happen infrequently, but `s3cmd` is much better for
interacting with the content on S3. I'll keep `s3cmd` configured to use my main
AWS admin account so it can create and modify buckets, but manually re-configure
`aws` to be whatever IAM user I need to be at the time -- for example, to attach
an SSL certificate to a dedicated user account, I need to configure `aws` to use
that account's access keys.

[aws-cli]: https://aur.archlinux.org/packages/aws-cli-git/
[s3cmd]: https://aur.archlinux.org/packages/s3cmd-git/

### S3

### Route 53

### SSL

Purchasing an SSL certificate requires a Signing Request:

```
openssl genrsa 2048 > pbrisbin.com.key
openssl req -new -key pbrisbin.com.key -out pbrisbin.com.csr
```

With this CSR, you can go ahead and purchase an SSL certificate. I recommend
buying a Comodo PositiveSSL Wildcard certificate for the following reasons:

- They're reasonably priced at ~$100/yr

That's about what you pay Netflix or Spotify.

- They conveniently include the top-level domain

Some wildcard certificates include only and exactly `*.pbrisbin.com` which
doesn't match `pbrisbin.com`, meaning I'd have to buy another certificate for
that domain.

- You can use a pre-built certificate chain

When you receive your certificate from Comodo, there will be a few other files
that need to be `cat`ed together to form a certificate chain. The names vary and
the order in which you `cat` them is important, thus making for some frustrating
trial and error. Since this chain doesn't differ from certificate to
certificate, you can download one that someone else's made:

```
curl -L https://gist.github.com/rwdaigle/5503531/raw/bundle.pem > pbrisbin.com-chain.crt
```

The files you receive from Comodo should contain a `STAR_pbrisbin.com.crt`, go
ahead and rename that to `prisbin.com.crt`. You should now have three files,
these are the important ones to hang onto and the ones we'll be using in the
next step:

- The private key, `pbrisbin.com.key`
- The public certificate, `pbrisbin.com.crt`
- The certificate chain, `pbrisbin.com-chain.crt`

Create an [Amazon IAM][iam] user with the following Policy:

[iam]: http://aws.amazon.com/iam/

```
{
"Version": "2015-04-01",
"Statement": [{
  "Effect": "Allow",
  "Action": ["*"],
  "Resource": ["*"]
  }]
}
```

Attach your certificate to this user with the following command:

```
aws iam upload-server-certificate \
  --server-certificate-name pbrisbin.com \
  --private-key file://pbrisbin.com.key \
  --certificate-body file://pbrisbin.com.crt \
  --certificate-chain file://pbrisbin.com-chain.crt \
  --path /cloudfront/pbrisbin.com/
```

### CloudFront

### Deployment
