#include <dns_sd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <poll.h>

static void queryCallback(DNSServiceRef sdRef, DNSServiceFlags flags, uint32_t interfaceIndex,
                          DNSServiceErrorType errorCode, const char *fullname, uint16_t rrtype,
                          uint16_t rrclass, uint16_t rdlen, const void *rdata, uint32_t ttl, void *context) {

    if (errorCode == kDNSServiceErr_NoError && rdlen > 1) {
        fprintf(stderr, "fullname = %s; rrtype = %d; rrclass = %d; rdlen = %d; ttl = %d\n",
                         fullname, rrtype, rrclass, rdlen, ttl);
        fprintf(stderr, "rdata = ");
        for (int i = 0; i < rdlen; i++) {
            fprintf(stderr, "%02x ", *(uint8_t*)(rdata + i));
        }
        fprintf(stderr, "\n");
        fprintf(stderr, "ascii = ");
        for (int i = 0; i < rdlen; i ++) {
            fprintf(stderr, "%c  ", *(char*)(rdata+i));
        }
        fprintf(stderr, "\n");
    }
}

int main(int argc, char **argv) {
    DNSServiceRef serviceRef;

    /* Send a query */
    fprintf(stderr, "dig -t TXT google.com\n");
    DNSServiceQueryRecord(&serviceRef, 0, 0, "google.com", kDNSServiceType_TXT,
                          kDNSServiceClass_IN, queryCallback, NULL);

    /* Wait for activity on the fd indicating a result */
    struct pollfd fds[1];
    fds[0].fd = DNSServiceRefSockFD(serviceRef);
    fds[0].events = POLLRDNORM;
    if (poll(fds, 1, 10000) != 1) {
        fprintf(stderr, "No activity on the DNS fd\n");
        exit(1);
    }
    
    DNSServiceProcessResult(serviceRef);
    DNSServiceRefDeallocate(serviceRef);

    fprintf(stderr, "dig -t MX google.com\n");
    DNSServiceQueryRecord(&serviceRef, 0, 0, "google.com", kDNSServiceType_MX,
                          kDNSServiceClass_IN, queryCallback, NULL);

    DNSServiceProcessResult(serviceRef);
    DNSServiceRefDeallocate(serviceRef);

    fprintf(stderr, "dig -t NS google.com\n");
    DNSServiceQueryRecord(&serviceRef, 0, 0, "google.com", kDNSServiceType_NS,
                          kDNSServiceClass_IN, queryCallback, NULL);

    DNSServiceProcessResult(serviceRef);
    DNSServiceRefDeallocate(serviceRef);
    return 0;
}

