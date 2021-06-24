// gcc -lxcb -lxcb-res -o xcb-client-id main.c

#include <stdio.h>
#include <stdlib.h>
#include <xcb/xcb.h>
#include <xcb/res.h>

int main(int argc, char *argv[]) {
    if (argc != 2) {
        return -1;
    }

    int screen;
    xcb_connection_t *conn = xcb_connect(NULL, &screen);

    xcb_res_client_id_spec_t spec = {0};
    spec.client = strtol(argv[1], NULL, 0);
    spec.mask = XCB_RES_CLIENT_ID_MASK_LOCAL_CLIENT_PID;

    xcb_generic_error_t *err = NULL;
    xcb_res_query_client_ids_cookie_t cookie = xcb_res_query_client_ids(conn, 1, &spec);
    xcb_res_query_client_ids_reply_t *reply = xcb_res_query_client_ids_reply(conn, cookie, &err);

    xcb_disconnect(conn);

    if (reply == NULL) {
        return -1;
    }

    uint32_t *pid = NULL;
    xcb_res_client_id_value_iterator_t it = xcb_res_query_client_ids_ids_iterator(reply);
    for (; it.rem; xcb_res_client_id_value_next(&it)) {
        spec = it.data->spec;
        if (spec.mask & XCB_RES_CLIENT_ID_MASK_LOCAL_CLIENT_PID) {
            pid = xcb_res_client_id_value_value(it.data);
            break;
        }
    }

    if (pid == NULL) {
        free(reply);
        return -1;
    }

    printf("%d\n", *pid);

    free(reply);
}
